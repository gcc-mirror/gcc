/* Gcov.c: prepend line execution counts and branch probabilities to a
   source file.
   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by James E. Wilson of Cygnus Support.
   Mangled by Bob Manson of Cygnus Support.

Gcov is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Gcov is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Gcov; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* ??? The code in final.c that produces the struct bb assumes that there is
   no padding between the fields.  This is not necessary true.  The current
   code can only be trusted if longs and pointers are the same size.  */

/* ??? No need to print an execution count on every line, could just print
   it on the first line of each block, and only print it on a subsequent
   line in the same block if the count changes.  */

/* ??? Print a list of the ten blocks with the highest execution counts,
   and list the line numbers corresponding to those blocks.  Also, perhaps
   list the line numbers with the highest execution counts, only printing
   the first if there are several which are all listed in the same block.  */

/* ??? Should have an option to print the number of basic blocks, and the
   percent of them that are covered.  */

/* ??? Does not correctly handle the case where two .bb files refer to the
   same included source file.  For example, if one has a short file containing
   only inline functions, which is then included in two other files, then
   there will be two .bb files which refer to the include file, but there
   is no way to get the total execution counts for the included file, can
   only get execution counts for one or the other of the including files.  */

#include "config.h"
#include "system.h"
#include "intl.h"
#include "version.h"
#undef abort

#include <getopt.h>

typedef HOST_WIDEST_INT gcov_type;
#include "gcov-io.h"

/* The .bb file format consists of several lists of 4-byte integers
   which are the line numbers of each basic block in the file.  Each
   list is terminated by a zero.  These lists correspond to the basic
   blocks in the reconstructed program flow graph.

   A line number of -1 indicates that a source file name (padded to a
   long boundary) follows.  The padded file name is followed by
   another -1 to make it easy to scan past file names.  A -2 indicates
   that a function name (padded to a long boundary) follows; the name
   is followed by another -2 to make it easy to scan past the function
   name.

   The .bbg file contains enough info to enable gcov to reconstruct the
   program flow graph.  The first word is the number of basic blocks,
   the second word is the number of arcs, followed by the list of arcs
   (source bb, dest bb pairs), then a -1, then the number of instrumented
   arcs followed by the instrumented arcs, followed by another -1.  This
   is repeated for each function.

   The .da file contains the execution count for each instrumented branch.

   The .bb and .bbg files are created by giving GCC the -ftest-coverage option,
   and the .da files are created when an executable compiled with
   -fprofile-arcs is run.  */

/* The functions in this file for creating and solution program flow graphs
   are very similar to functions in the gcc source file profile.c.  */

/* This is the size of the buffer used to read in source file lines.  */

#define STRING_SIZE 200

/* One copy of this structure is created for each source file mentioned in the
   .bb file.  */

struct sourcefile
{
  char *name;
  int maxlineno;
  struct sourcefile *next;
};

/* This points to the head of the sourcefile structure list.  */

struct sourcefile *sources;

/* One of these is dynamically created whenever we identify an arc in the
   function.  */

struct adj_list {
  int source;
  int target;
  gcov_type arc_count;
  unsigned int count_valid : 1;
  unsigned int on_tree : 1;
  unsigned int fake : 1;
  unsigned int fall_through : 1;
#if 0
  /* Not needed for gcov, but defined in profile.c.  */
  rtx branch_insn;
#endif
  struct adj_list *pred_next;
  struct adj_list *succ_next;
};

/* Count the number of basic blocks, and create an array of these structures,
   one for each bb in the function.  */

struct bb_info {
  struct adj_list *succ;
  struct adj_list *pred;
  gcov_type succ_count;
  gcov_type pred_count;
  gcov_type exec_count;
  unsigned int count_valid : 1;
  unsigned int on_tree : 1;
#if 0
  /* Not needed for gcov, but defined in profile.c.  */
  rtx first_insn;
#endif
};

/* When outputting branch probabilities, one of these structures is created
   for each branch/call.  */

struct arcdata
{
  gcov_type hits;
  gcov_type total;
  int call_insn;
  struct arcdata *next;
};

/* Used to save the list of bb_graphs, one per function.  */

struct bb_info_list {
  /* Indexed by block number, holds the basic block graph for one function.  */
  struct bb_info *bb_graph;
  int num_blocks;
  struct bb_info_list *next;
};

/* Holds a list of function basic block graphs.  */

static struct bb_info_list *bb_graph_list = 0;

/* Name and file pointer of the input file for the basic block graph.  */

static char *bbg_file_name;
static FILE *bbg_file;

/* Name and file pointer of the input file for the arc count data.  */

static char *da_file_name;
static FILE *da_file;

/* Name and file pointer of the input file for the basic block line counts.  */

static char *bb_file_name;
static FILE *bb_file;

/* Holds the entire contents of the bb_file read into memory.  */

static char *bb_data;

/* Size of bb_data array in longs.  */

static long bb_data_size;

/* Name and file pointer of the output file.  */

static char *gcov_file_name;
static FILE *gcov_file;

/* Name of the file mentioned on the command line.  */

static char *input_file_name = 0;

/* Output branch probabilities if true.  */

static int output_branch_probs = 0;

/* Output a gcov file if this is true.  This is on by default, and can
   be turned off by the -n option.  */

static int output_gcov_file = 1;

/* For included files, make the gcov output file name include the name of
   the input source file.  For example, if x.h is included in a.c, then the
   output file name is a.c.x.h.gcov instead of x.h.gcov.  This works only
   when a single source file is specified.  */

static int output_long_names = 0;

/* Output summary info for each function.  */

static int output_function_summary = 0;

/* Object directory file prefix.  This is the directory where .bb and .bbg
   files are looked for, if non-zero.  */

static char *object_directory = 0;

/* Output the number of times a branch was taken as opposed to the percentage
   of times it was taken.  Turned on by the -c option */

static int output_branch_counts = 0;

/* Forward declarations.  */
static void process_args PARAMS ((int, char **));
static void open_files PARAMS ((void));
static void read_files PARAMS ((void));
static void scan_for_source_files PARAMS ((void));
static void output_data PARAMS ((void));
static void print_usage PARAMS ((int)) ATTRIBUTE_NORETURN;
static void print_version PARAMS ((void)) ATTRIBUTE_NORETURN;
static void init_arc PARAMS ((struct adj_list *, int, int, struct bb_info *));
static struct adj_list *reverse_arcs PARAMS ((struct adj_list *));
static void create_program_flow_graph PARAMS ((struct bb_info_list *));
static void solve_program_flow_graph PARAMS ((struct bb_info_list *));
static void calculate_branch_probs PARAMS ((struct bb_info_list *, int,
					    struct arcdata **, int));
static void function_summary PARAMS ((void));

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  gcc_init_libintl ();

  process_args (argc, argv);

  open_files ();

  read_files ();

  scan_for_source_files ();

  output_data ();

  return 0;
}

static void fnotice PARAMS ((FILE *, const char *, ...)) ATTRIBUTE_PRINTF_2;
static void
fnotice VPARAMS ((FILE *file, const char *msgid, ...))
{
  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, FILE *, file);
  VA_FIXEDARG (ap, const char *, msgid);

  vfprintf (file, _(msgid), ap);
  VA_CLOSE (ap);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */
extern void fancy_abort PARAMS ((void)) ATTRIBUTE_NORETURN;

void
fancy_abort ()
{
  fnotice (stderr, "Internal gcov abort.\n");
  exit (FATAL_EXIT_CODE);
}

/* Print a usage message and exit.  If ERROR_P is nonzero, this is an error,
   otherwise the output of --help.  */

static void
print_usage (error_p)
     int error_p;
{
  FILE *file = error_p ? stderr : stdout;
  int status = error_p ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE;
  fnotice (file, "Usage: gcov [OPTION]... SOURCEFILE\n\n");
  fnotice (file, "Print code coverage information.\n\n");
  fnotice (file, "  -h, --help                      Print this help, then exit\n");
  fnotice (file, "  -v, --version                   Print version number, then exit\n");
  fnotice (file, "  -b, --branch-probabilities      Include branch probabilities in output\n");
  fnotice (file, "  -c, --branch-counts             Given counts of branches taken\n\
                                    rather than percentages\n");
  fnotice (file, "  -n, --no-output                 Do not create an output file\n");
  fnotice (file, "  -l, --long-file-names           Use long output file names for included\n\
                                    source files\n");
  fnotice (file, "  -f, --function-summaries        Output summaries for each function\n");
  fnotice (file, "  -o, --object-directory OBJDIR   Search for object files in OBJDIR\n");
  fnotice (file, "\nFor bug reporting instructions, please see:\n%s.\n",
	   GCCBUGURL);
  exit (status);
}

/* Print version information and exit.  */

static void
print_version ()
{
  fnotice (stdout, "gcov (GCC) %s\n", version_string);
  fnotice (stdout, "Copyright (C) 2001 Free Software Foundation, Inc.\n");
  fnotice (stdout,
	   "This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
  exit (SUCCESS_EXIT_CODE);
}

static const struct option options[] =
{
  { "help",                 no_argument,       NULL, 'h' },
  { "version",              no_argument,       NULL, 'v' },
  { "branch-probabilities", no_argument,       NULL, 'b' },
  { "branch-counts",        no_argument,       NULL, 'c' },
  { "no-output",            no_argument,       NULL, 'n' },
  { "long-file-names",      no_argument,       NULL, 'l' },
  { "function-summaries",   no_argument,       NULL, 'f' },
  { "object-directory",     required_argument, NULL, 'o' }
};

/* Parse the command line.  */

static void
process_args (argc, argv)
     int argc;
     char **argv;
{
  int opt;

  while ((opt = getopt_long (argc, argv, "hvbclnfo:", options, NULL)) != -1)
    {
      switch (opt)
	{
	case 'h':
	  print_usage (false);
	  /* print_usage will exit.  */
	case 'v':
	  print_version ();
	  /* print_version will exit.  */
	case 'b':
	  output_branch_probs = 1;
	  break;
	case 'c':
	  output_branch_counts = 1;
	  break;
	case 'n':
	  output_gcov_file = 0;
	  break;
	case 'l':
	  output_long_names = 1;
	  break;
	case 'f':
	  output_function_summary = 1;
	  break;
	case 'o':
	  object_directory = optarg;
	  break;
	default:
	  print_usage (true);
	  /* print_usage will exit.  */
	}
    }

  if (optind != argc - 1)
    print_usage (true);

  input_file_name = argv[optind];
}


/* Find and open the .bb, .da, and .bbg files.  */

static void
open_files ()
{
  int count, objdir_count;
  char *cptr;

  /* Determine the names of the .bb, .bbg, and .da files.  Strip off the
     extension, if any, and append the new extensions.  */
  count = strlen (input_file_name);
  if (object_directory)
    objdir_count = strlen (object_directory);
  else
    objdir_count = 0;

  da_file_name = xmalloc (count + objdir_count + 4);
  bb_file_name = xmalloc (count + objdir_count + 4);
  bbg_file_name = xmalloc (count + objdir_count + 5);

  if (object_directory)
    {
      strcpy (da_file_name, object_directory);
      strcpy (bb_file_name, object_directory);
      strcpy (bbg_file_name, object_directory);

      if (object_directory[objdir_count - 1] != '/')
	{
	  strcat (da_file_name, "/");
	  strcat (bb_file_name, "/");
	  strcat (bbg_file_name, "/");
	}

      cptr = strrchr (input_file_name, '/');
      if (cptr)
	{
	  strcat (da_file_name, cptr + 1);
	  strcat (bb_file_name, cptr + 1);
	  strcat (bbg_file_name, cptr + 1);
	}
      else
	{
	  strcat (da_file_name, input_file_name);
	  strcat (bb_file_name, input_file_name);
	  strcat (bbg_file_name, input_file_name);
	}
    }
  else
    {
      strcpy (da_file_name, input_file_name);
      strcpy (bb_file_name, input_file_name);
      strcpy (bbg_file_name, input_file_name);
    }

  cptr = strrchr (bb_file_name, '.');
  if (cptr)
    strcpy (cptr, ".bb");
  else
    strcat (bb_file_name, ".bb");

  cptr = strrchr (da_file_name, '.');
  if (cptr)
    strcpy (cptr, ".da");
  else
    strcat (da_file_name, ".da");

  cptr = strrchr (bbg_file_name, '.');
  if (cptr)
    strcpy (cptr, ".bbg");
  else
    strcat (bbg_file_name, ".bbg");

  bb_file = fopen (bb_file_name, "rb");
  if (bb_file == NULL)
    {
      fnotice (stderr, "Could not open basic block file %s.\n", bb_file_name);
      exit (FATAL_EXIT_CODE);
    }

  /* If none of the functions in the file were executed, then there won't
     be a .da file.  Just assume that all counts are zero in this case.  */
  da_file = fopen (da_file_name, "rb");
  if (da_file == NULL)
    {
      fnotice (stderr, "Could not open data file %s.\n", da_file_name);
      fnotice (stderr, "Assuming that all execution counts are zero.\n");
    }

  bbg_file = fopen (bbg_file_name, "rb");
  if (bbg_file == NULL)
    {
      fnotice (stderr, "Could not open program flow graph file %s.\n",
	       bbg_file_name);
      exit (FATAL_EXIT_CODE);
    }

  /* Check for empty .bbg file.  This indicates that there is no executable
     code in this source file.  */
  /* Set the EOF condition if at the end of file.  */
  ungetc (getc (bbg_file), bbg_file);
  if (feof (bbg_file))
    {
      fnotice (stderr, "No executable code associated with file %s.\n",
	       input_file_name);
      exit (FATAL_EXIT_CODE);
    }
}

/* Initialize a new arc.  */

static void
init_arc (arcptr, source, target, bb_graph)
     struct adj_list *arcptr;
     int source, target;
     struct bb_info *bb_graph;
{
  arcptr->target = target;
  arcptr->source = source;

  arcptr->arc_count = 0;
  arcptr->count_valid = 0;
  arcptr->on_tree = 0;
  arcptr->fake = 0;
  arcptr->fall_through = 0;

  arcptr->succ_next = bb_graph[source].succ;
  bb_graph[source].succ = arcptr;
  bb_graph[source].succ_count++;

  arcptr->pred_next = bb_graph[target].pred;
  bb_graph[target].pred = arcptr;
  bb_graph[target].pred_count++;
}


/* Reverse the arcs on an arc list.  */

static struct adj_list *
reverse_arcs (arcptr)
     struct adj_list *arcptr;
{
  struct adj_list *prev = 0;
  struct adj_list *next;

  for ( ; arcptr; arcptr = next)
    {
      next = arcptr->succ_next;
      arcptr->succ_next = prev;
      prev = arcptr;
    }

  return prev;
}


/* Construct the program flow graph from the .bbg file, and read in the data
   in the .da file.  */

static void
create_program_flow_graph (bptr)
     struct bb_info_list *bptr;
{
  long num_blocks, number_arcs, src, dest, flag_bits, num_arcs_per_block;
  int i;
  struct adj_list *arcptr;
  struct bb_info *bb_graph;

  /* Read the number of blocks.  */
  __read_long (&num_blocks, bbg_file, 4);

  /* Create an array of size bb number of bb_info structs.  */
  bb_graph = (struct bb_info *) xcalloc (num_blocks, sizeof (struct bb_info));

  bptr->bb_graph = bb_graph;
  bptr->num_blocks = num_blocks;

  /* Read and create each arc from the .bbg file.  */
  __read_long (&number_arcs, bbg_file, 4);
  for (i = 0; i < num_blocks; i++)
    {
      int j;

      __read_long (&num_arcs_per_block, bbg_file, 4);
      for (j = 0; j < num_arcs_per_block; j++)
	{
	  if (number_arcs-- < 0)
	    abort ();

	  src = i;
	  __read_long (&dest, bbg_file, 4);

	  arcptr = (struct adj_list *) xmalloc (sizeof (struct adj_list));
	  init_arc (arcptr, src, dest, bb_graph);

	  __read_long (&flag_bits, bbg_file, 4);
	  arcptr->on_tree = flag_bits & 0x1;
	  arcptr->fake = !! (flag_bits & 0x2);
	  arcptr->fall_through = !! (flag_bits & 0x4);
	}
    }

  if (number_arcs)
    abort ();

  /* Read and ignore the -1 separating the arc list from the arc list of the
     next function.  */
  __read_long (&src, bbg_file, 4);
  if (src != -1)
    abort ();

  /* Must reverse the order of all succ arcs, to ensure that they match
     the order of the data in the .da file.  */

  for (i = 0; i < num_blocks; i++)
    if (bb_graph[i].succ)
      bb_graph[i].succ = reverse_arcs (bb_graph[i].succ);

  /* For each arc not on the spanning tree, set its execution count from
     the .da file.  */

  /* The first count in the .da file is the number of times that the function
     was entered.  This is the exec_count for block zero.  */

  /* This duplicates code in branch_prob in profile.c.  */

  for (i = 0; i < num_blocks; i++)
    for (arcptr = bb_graph[i].succ; arcptr; arcptr = arcptr->succ_next)
      if (! arcptr->on_tree)
	{
	  gcov_type tmp_count = 0;
	  if (da_file && __read_gcov_type (&tmp_count, da_file, 8))
	    abort ();

	  arcptr->arc_count = tmp_count;
	  arcptr->count_valid = 1;
	  bb_graph[i].succ_count--;
	  bb_graph[arcptr->target].pred_count--;
	}
}

static void
solve_program_flow_graph (bptr)
     struct bb_info_list *bptr;
{
  int passes, changes;
  gcov_type total;
  int i;
  struct adj_list *arcptr;
  struct bb_info *bb_graph;
  int num_blocks;

  num_blocks = bptr->num_blocks;
  bb_graph = bptr->bb_graph;

  /* For every block in the file,
     - if every exit/entrance arc has a known count, then set the block count
     - if the block count is known, and every exit/entrance arc but one has
       a known execution count, then set the count of the remaining arc

     As arc counts are set, decrement the succ/pred count, but don't delete
     the arc, that way we can easily tell when all arcs are known, or only
     one arc is unknown.  */

  /* The order that the basic blocks are iterated through is important.
     Since the code that finds spanning trees starts with block 0, low numbered
     arcs are put on the spanning tree in preference to high numbered arcs.
     Hence, most instrumented arcs are at the end.  Graph solving works much
     faster if we propagate numbers from the end to the start.

     This takes an average of slightly more than 3 passes.  */

  changes = 1;
  passes = 0;
  while (changes)
    {
      passes++;
      changes = 0;

      for (i = num_blocks - 1; i >= 0; i--)
	{
	  if (! bb_graph[i].count_valid)
	    {
	      if (bb_graph[i].succ_count == 0)
		{
		  total = 0;
		  for (arcptr = bb_graph[i].succ; arcptr;
		       arcptr = arcptr->succ_next)
		    total += arcptr->arc_count;
		  bb_graph[i].exec_count = total;
		  bb_graph[i].count_valid = 1;
		  changes = 1;
		}
	      else if (bb_graph[i].pred_count == 0)
		{
		  total = 0;
		  for (arcptr = bb_graph[i].pred; arcptr;
		       arcptr = arcptr->pred_next)
		    total += arcptr->arc_count;
		  bb_graph[i].exec_count = total;
		  bb_graph[i].count_valid = 1;
		  changes = 1;
		}
	    }
	  if (bb_graph[i].count_valid)
	    {
	      if (bb_graph[i].succ_count == 1)
		{
		  total = 0;
		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  for (arcptr = bb_graph[i].succ; arcptr;
		       arcptr = arcptr->succ_next)
		    total += arcptr->arc_count;
		  /* Calculate count for remaining arc by conservation.  */
		  total = bb_graph[i].exec_count - total;
		  /* Search for the invalid arc, and set its count.  */
		  for (arcptr = bb_graph[i].succ; arcptr;
		       arcptr = arcptr->succ_next)
		    if (! arcptr->count_valid)
		      break;
		  if (! arcptr)
		    abort ();
		  arcptr->count_valid = 1;
		  arcptr->arc_count = total;
		  bb_graph[i].succ_count--;

		  bb_graph[arcptr->target].pred_count--;
		  changes = 1;
		}
	      if (bb_graph[i].pred_count == 1)
		{
		  total = 0;
		  /* One of the counts will be invalid, but it is zero,
		     so adding it in also doesn't hurt.  */
		  for (arcptr = bb_graph[i].pred; arcptr;
		       arcptr = arcptr->pred_next)
		    total += arcptr->arc_count;
		  /* Calculate count for remaining arc by conservation.  */
		  total = bb_graph[i].exec_count - total;
		  /* Search for the invalid arc, and set its count.  */
		  for (arcptr = bb_graph[i].pred; arcptr;
		       arcptr = arcptr->pred_next)
		    if (! arcptr->count_valid)
		      break;
		  if (! arcptr)
		    abort ();
		  arcptr->count_valid = 1;
		  arcptr->arc_count = total;
		  bb_graph[i].pred_count--;

		  bb_graph[arcptr->source].succ_count--;
		  changes = 1;
		}
	    }
	}
    }

  /* If the graph has been correctly solved, every block will have a
     succ and pred count of zero.  */
  for (i = 0; i < num_blocks; i++)
    if (bb_graph[i].succ_count || bb_graph[i].pred_count)
      abort ();
}


static void
read_files ()
{
  struct stat buf;
  struct bb_info_list *list_end = 0;
  struct bb_info_list *b_ptr;
  long total;

  /* Read and ignore the first word of the .da file, which is the count of
     how many numbers follow.  */
  if (da_file && __read_long (&total, da_file, 8))
    abort ();

  while (! feof (bbg_file))
    {
      b_ptr = (struct bb_info_list *) xmalloc (sizeof (struct bb_info_list));

      b_ptr->next = 0;
      if (list_end)
	list_end->next = b_ptr;
      else
	bb_graph_list = b_ptr;
      list_end = b_ptr;

      /* Read in the data in the .bbg file and reconstruct the program flow
	 graph for one function.  */
      create_program_flow_graph (b_ptr);

      /* Set the EOF condition if at the end of file.  */
      ungetc (getc (bbg_file), bbg_file);
    }

  /* Check to make sure the .da file data is valid.  */

  if (da_file)
    {
      if (feof (da_file))
	fnotice (stderr, ".da file contents exhausted too early\n");
      /* Should be at end of file now.  */
      if (__read_long (&total, da_file, 8) == 0)
	fnotice (stderr, ".da file contents not exhausted\n");
    }

  /* Calculate all of the basic block execution counts and branch
     taken probabilities.  */

  for (b_ptr = bb_graph_list; b_ptr; b_ptr = b_ptr->next)
    solve_program_flow_graph (b_ptr);

  /* Read in all of the data from the .bb file.   This info will be accessed
     sequentially twice.  */
  stat (bb_file_name, &buf);
  bb_data_size = buf.st_size / 4;

  bb_data = (char *) xmalloc ((unsigned) buf.st_size);
  fread (bb_data, sizeof (char), buf.st_size, bb_file);

  fclose (bb_file);
  if (da_file)
    fclose (da_file);
  fclose (bbg_file);
}


/* Scan the data in the .bb file to find all source files referenced,
   and the largest line number mentioned in each one.  */

static void
scan_for_source_files ()
{
  struct sourcefile *s_ptr = NULL;
  char *ptr;
  long count;
  long line_num;

  /* Search the bb_data to find:
     1) The number of sources files contained herein, and
     2) The largest line number for each source file.  */

  ptr = bb_data;
  sources = 0;
  for (count = 0; count < bb_data_size; count++)
    {
      __fetch_long (&line_num, ptr, 4);
      ptr += 4;
      if (line_num == -1)
	{
	  /* A source file name follows.  Check to see if we already have
	   a sourcefile structure for this file.  */
	  s_ptr = sources;
	  while (s_ptr && strcmp (s_ptr->name, ptr))
	    s_ptr = s_ptr->next;

	  if (s_ptr == 0)
	    {
	      /* No sourcefile structure for this file name exists, create
		 a new one, and append it to the front of the sources list.  */
	      s_ptr = (struct sourcefile *) xmalloc (sizeof(struct sourcefile));
	      s_ptr->name = xstrdup (ptr);
	      s_ptr->maxlineno = 0;
	      s_ptr->next = sources;
	      sources = s_ptr;
	    }

	  /* Scan past the file name.  */
	  {
	    long delim;
	    do {
	      count++;
	      __fetch_long (&delim, ptr, 4);
	      ptr += 4;
	    } while (delim != line_num);
	  }
	}
      else if (line_num == -2)
	{
	  long delim;

	  /* A function name follows.  Ignore it.  */
	  do {
	    count++;
	    __fetch_long (&delim, ptr, 4);
	    ptr += 4;
	  } while (delim != line_num);
	}
      /* There will be a zero before the first file name, in which case s_ptr
	 will still be uninitialized.  So, only try to set the maxlineno
	 field if line_num is non-zero.  */
      else if (line_num > 0)
	{
	  if (s_ptr->maxlineno <= line_num)
	    s_ptr->maxlineno = line_num + 1;
	}
      else if (line_num < 0)
	{
	  /* Don't know what this is, but it's garbage.  */
	  abort ();
	}
    }
}

/* For calculating coverage at the function level.  */

static int function_source_lines;
static int function_source_lines_executed;
static int function_branches;
static int function_branches_executed;
static int function_branches_taken;
static int function_calls;
static int function_calls_executed;
static char *function_name;

/* Calculate the branch taken probabilities for all arcs branches at the
   end of this block.  */

static void
calculate_branch_probs (current_graph, block_num, branch_probs, last_line_num)
     struct bb_info_list *current_graph;
     int block_num;
     struct arcdata **branch_probs;
     int last_line_num;
{
  gcov_type total;
  struct adj_list *arcptr;
  struct arcdata *end_ptr, *a_ptr;

  total = current_graph->bb_graph[block_num].exec_count;
  for (arcptr = current_graph->bb_graph[block_num].succ; arcptr;
       arcptr = arcptr->succ_next)
    {
      /* Ignore fall through arcs as they aren't really branches.  */

      if (arcptr->fall_through)
	continue;

      a_ptr = (struct arcdata *) xmalloc (sizeof (struct arcdata));
      a_ptr->total = total;
      if (total == 0)
          a_ptr->hits = 0;
      else
          a_ptr->hits = arcptr->arc_count;
      a_ptr->call_insn = arcptr->fake;

      if (output_function_summary)
	{
	  if (a_ptr->call_insn)
	    {
	      function_calls++;
	      if (a_ptr->total != 0)
		function_calls_executed++;
	    }
	  else
	    {
	      function_branches++;
	      if (a_ptr->total != 0)
		function_branches_executed++;
	      if (a_ptr->hits > 0)
		function_branches_taken++;
	    }
	}

      /* Append the new branch to the end of the list.  */
      a_ptr->next = 0;
      if (! branch_probs[last_line_num])
	branch_probs[last_line_num] = a_ptr;
      else
	{
	  end_ptr = branch_probs[last_line_num];
	  while (end_ptr->next != 0)
	    end_ptr = end_ptr->next;
	  end_ptr->next = a_ptr;
	}
    }
}

/* Output summary info for a function.  */

static void
function_summary ()
{
  if (function_source_lines)
    fnotice (stdout, "%6.2f%% of %d source lines executed in function %s\n",
	     (((double) function_source_lines_executed / function_source_lines)
	      * 100), function_source_lines, function_name);
  else
    fnotice (stdout, "No executable source lines in function %s\n",
	     function_name);

  if (output_branch_probs)
    {
      if (function_branches)
	{
	  fnotice (stdout, "%6.2f%% of %d branches executed in function %s\n",
		   (((double) function_branches_executed / function_branches)
		    * 100), function_branches, function_name);
	  fnotice (stdout,
		"%6.2f%% of %d branches taken at least once in function %s\n",
		   (((double) function_branches_taken / function_branches)
		    * 100), function_branches, function_name);
	}
      else
	fnotice (stdout, "No branches in function %s\n", function_name);
      if (function_calls)
	fnotice (stdout, "%6.2f%% of %d calls executed in function %s\n",
		 (((double) function_calls_executed / function_calls)
		  * 100), function_calls, function_name);
      else
	fnotice (stdout, "No calls in function %s\n", function_name);
    }
}

/* Calculate line execution counts, and output the data to a .tcov file.  */

static void
output_data ()
{
  /* When scanning data, this is true only if the data applies to the
     current source file.  */
  int this_file;
  /* An array indexed by line number which indicates how many times that line
     was executed.  */
  gcov_type *line_counts;
  /* An array indexed by line number which indicates whether the line was
     present in the bb file (i.e. whether it had code associate with it).
     Lines never executed are those which both exist, and have zero execution
     counts.  */
  char *line_exists;
  /* An array indexed by line number, which contains a list of branch
     probabilities, one for each branch on that line.  */
  struct arcdata **branch_probs = NULL;
  struct sourcefile *s_ptr;
  char *source_file_name;
  FILE *source_file;
  struct bb_info_list *current_graph;
  long count;
  char *cptr;
  long block_num;
  long line_num;
  long last_line_num = 0;
  int i;
  struct arcdata *a_ptr;
  /* Buffer used for reading in lines from the source file.  */
  char string[STRING_SIZE];
  /* For calculating coverage at the file level.  */
  int total_source_lines;
  int total_source_lines_executed;
  int total_branches;
  int total_branches_executed;
  int total_branches_taken;
  int total_calls;
  int total_calls_executed;

  /* Now, for each source file, allocate an array big enough to hold a count
     for each line.  Scan through the bb_data, and when the file name matches
     the current file name, then for each following line number, increment
     the line number execution count indicated by the execution count of
     the appropriate basic block.  */

  for (s_ptr = sources; s_ptr; s_ptr = s_ptr->next)
    {
      /* If this is a relative file name, and an object directory has been
	 specified, then make it relative to the object directory name.  */
      if (! IS_ABSOLUTE_PATHNAME (s_ptr->name)
	  && object_directory != 0
	  && *object_directory != '\0')
	{
	  int objdir_count = strlen (object_directory);
	  source_file_name = xmalloc (objdir_count + strlen (s_ptr->name) + 2);
	  strcpy (source_file_name, object_directory);
	  if (object_directory[objdir_count - 1] != '/')
	    source_file_name[objdir_count++] = '/';
	  strcpy (source_file_name + objdir_count, s_ptr->name);
	}
      else
	source_file_name = s_ptr->name;

      line_counts = (gcov_type *) xcalloc (sizeof (gcov_type), s_ptr->maxlineno);
      line_exists = xcalloc (1, s_ptr->maxlineno);
      if (output_branch_probs)
	branch_probs = (struct arcdata **)
	  xcalloc (sizeof (struct arcdata *), s_ptr->maxlineno);

      /* There will be a zero at the beginning of the bb info, before the
	 first list of line numbers, so must initialize block_num to 0.  */
      block_num = 0;
      this_file = 0;
      current_graph = 0;
      {
	/* Pointer into the bb_data, incremented while scanning the data.  */
	char *ptr = bb_data;
	for (count = 0; count < bb_data_size; count++)
	  {
	    long delim;

	    __fetch_long (&line_num, ptr, 4);
	    ptr += 4;
	    if (line_num == -1)
	      {
		/* Marks the beginning of a file name.  Check to see whether
		   this is the filename we are currently collecting data for.  */

		if (strcmp (s_ptr->name, ptr))
		  this_file = 0;
		else
		  this_file = 1;

		/* Scan past the file name.  */
		do {
		  count++;
		  __fetch_long (&delim, ptr, 4);
		  ptr += 4;
		} while (delim != line_num);
	      }
	    else if (line_num == -2)
	      {
		/* Marks the start of a new function.  Advance to the next
		   program flow graph.  */

		if (! current_graph)
		  current_graph = bb_graph_list;
		else
		  {
		    if (block_num == current_graph->num_blocks - 1)
		      /* Last block falls through to exit.  */
		      ;
		    else if (block_num == current_graph->num_blocks - 2)
		      {
			if (output_branch_probs && this_file)
			  calculate_branch_probs (current_graph, block_num,
						  branch_probs, last_line_num);
		      }
		    else
		      {
			fnotice (stderr,
				 "didn't use all bb entries of graph, function %s\n",
				 function_name);
			fnotice (stderr, "block_num = %ld, num_blocks = %d\n",
				 block_num, current_graph->num_blocks);
		      }

		    current_graph = current_graph->next;
		    block_num = 0;

		    if (output_function_summary && this_file)
		      function_summary ();
		  }

		if (output_function_summary)
		  {
		    function_source_lines = 0;
		    function_source_lines_executed = 0;
		    function_branches = 0;
		    function_branches_executed = 0;
		    function_branches_taken = 0;
		    function_calls = 0;
		    function_calls_executed = 0;
		  }

		/* Save the function name for later use.  */
		function_name = ptr;

		/* Scan past the file name.  */
		do {
		  count++;
		  __fetch_long (&delim, ptr, 4);
		  ptr += 4;
		} while (delim != line_num);
	      }
	    else if (line_num == 0)
	      {
		/* Marks the end of a block.  */

		if (block_num >= current_graph->num_blocks)
		  {
		    fnotice (stderr, "ERROR: too many basic blocks in .bb file %s\n",
			     function_name);
		    abort ();
		  }

		if (output_branch_probs && this_file)
		  calculate_branch_probs (current_graph, block_num,
					  branch_probs, last_line_num);

		block_num++;
	      }
	    else if (this_file)
	      {
		if (output_function_summary)
		  {
		    if (line_exists[line_num] == 0)
		      function_source_lines++;
		    if (line_counts[line_num] == 0
			&& current_graph->bb_graph[block_num].exec_count != 0)
		      function_source_lines_executed++;
		  }

		/* Accumulate execution data for this line number.  */

		line_counts[line_num]
		  += current_graph->bb_graph[block_num].exec_count;
		line_exists[line_num] = 1;
		last_line_num = line_num;
	      }
	  }
      }

      if (output_function_summary && this_file)
	function_summary ();

      /* Calculate summary test coverage statistics.  */

      total_source_lines = 0;
      total_source_lines_executed = 0;
      total_branches = 0;
      total_branches_executed = 0;
      total_branches_taken = 0;
      total_calls = 0;
      total_calls_executed = 0;

      for (count = 1; count < s_ptr->maxlineno; count++)
	{
	  if (line_exists[count])
	    {
	      total_source_lines++;
	      if (line_counts[count])
		total_source_lines_executed++;
	    }
	  if (output_branch_probs)
	    {
	      for (a_ptr = branch_probs[count]; a_ptr; a_ptr = a_ptr->next)
		{
		  if (a_ptr->call_insn)
		    {
		      total_calls++;
		      if (a_ptr->total != 0)
			total_calls_executed++;
		    }
		  else
		    {
		      total_branches++;
		      if (a_ptr->total != 0)
			total_branches_executed++;
		      if (a_ptr->hits > 0)
			total_branches_taken++;
		    }
		}
	    }
	}

      if (total_source_lines)
	fnotice (stdout,
		 "%6.2f%% of %d source lines executed in file %s\n",
		 (((double) total_source_lines_executed / total_source_lines)
		  * 100), total_source_lines, source_file_name);
      else
	fnotice (stdout, "No executable source lines in file %s\n",
		 source_file_name);

      if (output_branch_probs)
	{
	  if (total_branches)
	    {
	      fnotice (stdout, "%6.2f%% of %d branches executed in file %s\n",
		       (((double) total_branches_executed / total_branches)
			* 100), total_branches, source_file_name);
	      fnotice (stdout,
		    "%6.2f%% of %d branches taken at least once in file %s\n",
		       (((double) total_branches_taken / total_branches)
			* 100), total_branches, source_file_name);
	    }
	  else
	    fnotice (stdout, "No branches in file %s\n", source_file_name);
	  if (total_calls)
	    fnotice (stdout, "%6.2f%% of %d calls executed in file %s\n",
		     (((double) total_calls_executed / total_calls)
		      * 100), total_calls, source_file_name);
	  else
	    fnotice (stdout, "No calls in file %s\n", source_file_name);
	}

      if (output_gcov_file)
	{
	  /* Now the statistics are ready.  Read in the source file one line
	     at a time, and output that line to the gcov file preceded by
	     its execution count if non zero.  */

	  source_file = fopen (source_file_name, "r");
	  if (source_file == NULL)
	    {
	      fnotice (stderr, "Could not open source file %s.\n",
		       source_file_name);
	      free (line_counts);
	      free (line_exists);
	      continue;
	    }

	  count = strlen (source_file_name);
	  cptr = strrchr (s_ptr->name, '/');
	  if (cptr)
	    cptr = cptr + 1;
	  else
	    cptr = s_ptr->name;
	  if (output_long_names && strcmp (cptr, input_file_name))
	    {
	      gcov_file_name = xmalloc (count + 7 + strlen (input_file_name));

	      cptr = strrchr (input_file_name, '/');
	      if (cptr)
		strcpy (gcov_file_name, cptr + 1);
	      else
		strcpy (gcov_file_name, input_file_name);

	      strcat (gcov_file_name, ".");

	      cptr = strrchr (source_file_name, '/');
	      if (cptr)
		strcat (gcov_file_name, cptr + 1);
	      else
		strcat (gcov_file_name, source_file_name);
	    }
	  else
	    {
	      gcov_file_name = xmalloc (count + 6);
	      cptr = strrchr (source_file_name, '/');
	      if (cptr)
		strcpy (gcov_file_name, cptr + 1);
	      else
		strcpy (gcov_file_name, source_file_name);
	    }

	  /* Don't strip off the ending for compatibility with tcov, since
	     this results in confusion if there is more than one file with
	     the same basename, e.g. tmp.c and tmp.h.  */
	  strcat (gcov_file_name, ".gcov");

	  gcov_file = fopen (gcov_file_name, "w");

	  if (gcov_file == NULL)
	    {
	      fnotice (stderr, "Could not open output file %s.\n",
		       gcov_file_name);
	      fclose (source_file);
	      free (line_counts);
	      free (line_exists);
	      continue;
	    }

	  fnotice (stdout, "Creating %s.\n", gcov_file_name);

	  for (count = 1; count < s_ptr->maxlineno; count++)
	    {
	      char *retval;
	      int len;

	      retval = fgets (string, STRING_SIZE, source_file);

	      /* For lines which don't exist in the .bb file, print nothing
		 before the source line.  For lines which exist but were never
		 executed, print ###### before the source line.  Otherwise,
		 print the execution count before the source line.  */
	      /* There are 16 spaces of indentation added before the source
		 line so that tabs won't be messed up.  */
	      if (line_exists[count])
		{
		  if (line_counts[count])
		    {
		      char c[20];
		      sprintf (c, HOST_WIDEST_INT_PRINT_DEC, (HOST_WIDEST_INT)line_counts[count]);
		      fprintf (gcov_file, "%12s    %s", c,
			       string);
		    }
		  else
		    fprintf (gcov_file, "      ######    %s", string);
		}
	      else
		fprintf (gcov_file, "\t\t%s", string);

	      /* In case the source file line is larger than our buffer, keep
		 reading and outputting lines until we get a newline.  */
	      len = strlen (string);
	      while ((len == 0 || string[strlen (string) - 1] != '\n')
		     && retval != NULL)
		{
		  retval = fgets (string, STRING_SIZE, source_file);
		  fputs (string, gcov_file);
		}

	      if (output_branch_probs)
		{
		  for (i = 0, a_ptr = branch_probs[count]; a_ptr;
		       a_ptr = a_ptr->next, i++)
		    {
		      if (a_ptr->call_insn)
			{
			  if (a_ptr->total == 0)
			    fnotice (gcov_file, "call %d never executed\n", i);
		            else
			      {
				if (output_branch_counts)
				  {
				    char c[20];
				    sprintf (c, HOST_WIDEST_INT_PRINT_DEC,
					     a_ptr->total - a_ptr->hits);
				    fnotice (gcov_file,
					     "call %d returns = %s\n", i, c);
				  }
			        else
				  {
				    char c[20];
				    sprintf (c, HOST_WIDEST_INT_PRINT_DEC,
					     100 - ((a_ptr->hits * 100)
						    + (a_ptr->total >> 1))
					     / a_ptr->total);
				    fnotice (gcov_file,
					     "call %d returns = %s%%\n", i, c);
				  }
			      }
			}
		      else
			{
			  if (a_ptr->total == 0)
			    fnotice (gcov_file, "branch %d never executed\n",
				     i);
			  else
			    {
			      if (output_branch_counts)
				{
				  char c[20];
				  sprintf (c, HOST_WIDEST_INT_PRINT_DEC,
					   a_ptr->hits);
				  fnotice (gcov_file,
					   "branch %d taken = %s\n", i, c);
				}
			      else
				{
				  char c[20];
				  sprintf (c, HOST_WIDEST_INT_PRINT_DEC,
					   ((a_ptr->hits * 100)
					    + (a_ptr->total >> 1))
					   / a_ptr->total);
                                fnotice (gcov_file,
                                         "branch %d taken = %s%%\n", i, c);
				}
			    }
			}
		   }
	      }

	      /* Gracefully handle errors while reading the source file.  */
	      if (retval == NULL)
		{
		  fnotice (stderr,
			   "Unexpected EOF while reading source file %s.\n",
			   source_file_name);
		  break;
		}
	    }

	  /* Handle all remaining source lines.  There may be lines
	     after the last line of code.  */

	  {
	    char *retval = fgets (string, STRING_SIZE, source_file);
	    while (retval != NULL)
	      {
		int len;

		fprintf (gcov_file, "\t\t%s", string);

		/* In case the source file line is larger than our buffer, keep
		   reading and outputting lines until we get a newline.  */
		len = strlen (string);
		while ((len == 0 || string[strlen (string) - 1] != '\n')
		       && retval != NULL)
		  {
		    retval = fgets (string, STRING_SIZE, source_file);
		    fputs (string, gcov_file);
		  }

		retval = fgets (string, STRING_SIZE, source_file);
	      }
	  }

	  fclose (source_file);
	  fclose (gcov_file);
	}

      free (line_counts);
      free (line_exists);
    }
}
