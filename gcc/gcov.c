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

struct adj_list
{
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

struct bb_info
{
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

struct bb_info_list
{
  /* Indexed by block number, holds the basic block graph for one function.  */
  struct bb_info *bb_graph;
  int num_blocks;
  struct bb_info_list *next;
};

/* Used to hold information about each line.  */
struct line_info
{
  gcov_type count;	      /* execution count */
  struct arcdata *branches;   /* list of branch probabilities for line.  */
  unsigned exists : 1;	      /* has code associated with it.  */
};
  
struct coverage
{
  int lines;
  int lines_executed;
  
  int branches;
  int branches_executed;
  int branches_taken;
  
  int calls;
  int calls_executed;
  
  char *name;
};

/* Holds a list of function basic block graphs.  */

static struct bb_info_list *bb_graph_list = 0;

/* Modification time of data files.  */

static time_t bb_file_time;

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

/* Object directory file prefix.  This is the directory/file
   where .bb and .bbg files are looked for, if nonzero.  */

static char *object_directory = 0;

/* Preserve all pathname components. Needed when object files and
   source files are in subdirectories.  */
static int preserve_paths = 0;

/* Output the number of times a branch was taken as opposed to the percentage
   of times it was taken.  Turned on by the -c option */

static int output_branch_counts = 0;

/* Forward declarations.  */
static void process_args PARAMS ((int, char **));
static void open_files PARAMS ((void));
static void read_files PARAMS ((void));
static void scan_for_source_files PARAMS ((void));
static void output_data PARAMS ((struct sourcefile *));
static void print_usage PARAMS ((int)) ATTRIBUTE_NORETURN;
static void print_version PARAMS ((void)) ATTRIBUTE_NORETURN;
static void init_arc PARAMS ((struct adj_list *, int, int, struct bb_info *));
static struct adj_list *reverse_arcs PARAMS ((struct adj_list *));
static gcov_type *read_profile PARAMS ((char *, long, int));
static void create_program_flow_graph PARAMS ((struct bb_info_list *));
static void solve_program_flow_graph PARAMS ((struct bb_info_list *));
static void accumulate_branch_counts PARAMS ((struct coverage *,
					      struct arcdata *));
static void calculate_branch_probs PARAMS ((struct bb_info *,
					    struct line_info *,
					    struct coverage *));
static void function_summary PARAMS ((struct coverage *, const char *));
static void init_line_info PARAMS ((struct line_info *,
				    struct coverage *, long));
static void output_line_info PARAMS ((FILE *, const struct line_info *,
				      const struct coverage *, long));
static char *make_gcov_file_name PARAMS ((char *));
static const char *format_hwint PARAMS ((HOST_WIDEST_INT, HOST_WIDEST_INT,
					 int));

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  struct sourcefile *s_ptr;
  
  gcc_init_libintl ();

  process_args (argc, argv);

  open_files ();

  read_files ();

  scan_for_source_files ();

  for (s_ptr = sources; s_ptr; s_ptr = s_ptr->next)
    output_data (s_ptr);

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
  fnotice (file, "  -o, --object-directory DIR|FILE Search for object files in DIR or called FILE\n");
  fnotice (file, "  -p, --preserve-paths            Preserve all pathname components\n");
  fnotice (file, "\nFor bug reporting instructions, please see:\n%s.\n",
	   bug_report_url);
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
  { "preserve-paths",       no_argument,       NULL, 'p' },
  { "object-directory",     required_argument, NULL, 'o' },
  { "object-file",          required_argument, NULL, 'o' },
};

/* Parse the command line.  */

static void
process_args (argc, argv)
     int argc;
     char **argv;
{
  int opt;

  while ((opt = getopt_long (argc, argv, "hvbclnfo:p", options, NULL)) != -1)
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
	case 'p':
	  preserve_paths = 1;
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


/* Find and open the .bb, .da, and .bbg files. If OBJECT_DIRECTORY is
   not specified, these are looked for in the current directory, and
   named from the basename of the input_file_name sans extension. If
   OBJECT_DIRECTORY is specified and is a directory, the files are in
   that directory, but named from the basename of the input_file_name,
   sans extension. Otherwise OBJECT_DIRECTORY is taken to be the name
   of the object *file*, and the data files are named from that.  */

static void
open_files ()
{
  char *cptr;
  char *name;
  int length = strlen (input_file_name);
  int base;
  
  if (object_directory && object_directory[0])
    {
      struct stat status;

      length += strlen (object_directory) + 2;
      name = xmalloc (length);
      name[0] = 0;
      
      base = !stat (object_directory, &status) && S_ISDIR (status.st_mode);
      strcat (name, object_directory);
      if (base && name[strlen (name) - 1] != '/')
	strcat (name, "/");
    }
  else
    {
      name = xmalloc (length + 1);
      name[0] = 0;
      base = 1;
    }
  
  if (base)
    {
      /* Append source file name */
      cptr = strrchr (input_file_name, '/');
      cptr = cptr ? cptr + 1 : input_file_name;

      strcat (name, cptr);
    }
  /* Remove the extension.  */
  cptr = strrchr (name, '.');
  if (cptr)
    *cptr = 0;
  
  length = strlen (name);
  da_file_name = xmalloc (length + 4);
  bb_file_name = xmalloc (length + 4);
  bbg_file_name = xmalloc (length + 5);

  strcpy (da_file_name, name);
  strcpy (bb_file_name, name);
  strcpy (bbg_file_name, name);
  strcpy (da_file_name + length, ".da");
  strcpy (bb_file_name + length, ".bb");
  strcpy (bbg_file_name + length, ".bbg");

  bb_file = fopen (bb_file_name, "rb");
  if (bb_file == NULL)
    {
      fnotice (stderr, "Could not open basic block file %s.\n", bb_file_name);
      exit (FATAL_EXIT_CODE);
    }

  bbg_file = fopen (bbg_file_name, "rb");
  if (bbg_file == NULL)
    {
      fnotice (stderr, "Could not open program flow graph file %s.\n",
	       bbg_file_name);
      exit (FATAL_EXIT_CODE);
    }
  
  {
    struct stat status;

    if (!fstat (fileno (bb_file), &status))
      bb_file_time = status.st_mtime;
  }
  
  /* If none of the functions in the file were executed, then there won't
     be a .da file.  Just assume that all counts are zero in this case.  */
  da_file = fopen (da_file_name, "rb");
  if (da_file == NULL)
    {
      fnotice (stderr, "Could not open data file %s.\n", da_file_name);
      fnotice (stderr, "Assuming that all execution counts are zero.\n");
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

/* Reads profiles from the .da file and compute a hybrid profile.  */

static gcov_type *
read_profile (function_name, cfg_checksum, instr_arcs)
     char *function_name;
     long cfg_checksum;
     int instr_arcs;
{
  int i;
  int okay = 1;
  gcov_type *profile;
  char *function_name_buffer;
  int function_name_buffer_len;

  profile = xmalloc (sizeof (gcov_type) * instr_arcs);
  function_name_buffer_len = strlen (function_name) + 1;
  function_name_buffer = xmalloc (function_name_buffer_len + 1);

  for (i = 0; i < instr_arcs; i++)
    profile[i] = 0;

  if (!da_file)
    return profile;

  rewind (da_file);
  while (1)
    {
      long magic, extra_bytes;
      long func_count;
      int i;

      if (__read_long (&magic, da_file, 4) != 0)
	break;

      if (magic != -123)
	{
	  okay = 0;
	  break;
	}

      if (__read_long (&func_count, da_file, 4) != 0)
	{
	  okay = 0;
	  break;
	}

      if (__read_long (&extra_bytes, da_file, 4) != 0)
	{
	  okay = 0;
	  break;
	}

      /* skip extra data emited by __bb_exit_func.  */
      fseek (da_file, extra_bytes, SEEK_CUR);

      for (i = 0; i < func_count; i++)
	{
	  long arc_count;
	  long chksum;
	  int j;

	  if (__read_gcov_string
	      (function_name_buffer, function_name_buffer_len, da_file,
	       -1) != 0)
	    {
	      okay = 0;
	      break;
	    }

	  if (__read_long (&chksum, da_file, 4) != 0)
	    {
	      okay = 0;
	      break;
	    }

	  if (__read_long (&arc_count, da_file, 4) != 0)
	    {
	      okay = 0;
	      break;
	    }

	  if (strcmp (function_name_buffer, function_name) != 0
	      || arc_count != instr_arcs || chksum != cfg_checksum)
	    {
	      /* skip */
	      if (fseek (da_file, arc_count * 8, SEEK_CUR) < 0)
		{
		  okay = 0;
		  break;
		}
	    }
	  else
	    {
	      gcov_type tmp;

	      for (j = 0; j < arc_count; j++)
		if (__read_gcov_type (&tmp, da_file, 8) != 0)
		  {
		    okay = 0;
		    break;
		  }
		else
		  {
		    profile[j] += tmp;
		  }
	    }
	}

      if (!okay)
	break;

    }

  free (function_name_buffer);

  if (!okay)
    {
      fprintf (stderr, ".da file corrupted!\n");
      free (profile);
      abort ();
    }

  return profile;
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
  long cfg_checksum;
  long instr_arcs = 0;
  gcov_type *profile;
  int profile_pos = 0;
  char *function_name;
  long function_name_len, tmp;

  /* Read function name.  */
  __read_long (&tmp, bbg_file, 4);   /* ignore -1.  */
  __read_long (&function_name_len, bbg_file, 4);
  function_name = xmalloc (function_name_len + 1);
  fread (function_name, 1, function_name_len + 1, bbg_file);

  /* Skip padding.  */
  tmp = (function_name_len + 1) % 4;

  if (tmp)
    fseek (bbg_file, 4 - tmp, SEEK_CUR);

  __read_long (&tmp, bbg_file, 4);   /* ignore -1.  */

  /* Read the cfg checksum.  */
  __read_long (&cfg_checksum, bbg_file, 4);

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
	  if (flag_bits & 0x1)
	    arcptr->on_tree++;
	  else
	    instr_arcs++;
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

  /* Read profile from the .da file.  */

  profile = read_profile (function_name, cfg_checksum, instr_arcs);

  /* For each arc not on the spanning tree, set its execution count from
     the .da file.  */

  /* The first count in the .da file is the number of times that the function
     was entered.  This is the exec_count for block zero.  */

  /* This duplicates code in branch_prob in profile.c.  */

  for (i = 0; i < num_blocks; i++)
    for (arcptr = bb_graph[i].succ; arcptr; arcptr = arcptr->succ_next)
      if (! arcptr->on_tree)
	{
	  arcptr->arc_count = profile[profile_pos++];
	  arcptr->count_valid = 1;
	  bb_graph[i].succ_count--;
	  bb_graph[arcptr->target].pred_count--;
	}
  free (profile);
  free (function_name);
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
	 field if line_num is nonzero.  */
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


/* Increment totals in FUNCTION according to arc A_PTR.  */

static void
accumulate_branch_counts (function, a_ptr)
     struct coverage *function;
     struct arcdata *a_ptr;
{
  if (a_ptr->call_insn)
    {
      function->calls++;
      if (a_ptr->total)
	function->calls_executed++;
    }
  else
    {
      function->branches++;
      if (a_ptr->total)
	function->branches_executed++;
      if (a_ptr->hits)
	function->branches_taken++;
    }
}

/* Calculate the branch taken probabilities for all arcs branches at the
   end of this block.  */

static void
calculate_branch_probs (block_ptr, line_info, function)
     struct bb_info *block_ptr;
     struct line_info *line_info;
     struct coverage *function;
{
  gcov_type total;
  struct adj_list *arcptr;

  total = block_ptr->exec_count;
  for (arcptr = block_ptr->succ; arcptr; arcptr = arcptr->succ_next)
    {
      struct arcdata *a_ptr;
      
      /* Ignore fall through arcs as they aren't really branches.  */
      if (arcptr->fall_through)
	continue;

      a_ptr = (struct arcdata *) xmalloc (sizeof (struct arcdata));
      a_ptr->total = total;
      a_ptr->hits = total ? arcptr->arc_count : 0;
      a_ptr->call_insn = arcptr->fake;

      if (function)
	accumulate_branch_counts (function, a_ptr);
      /* Prepend the new branch to the list.  */
      a_ptr->next = line_info->branches;
      line_info->branches = a_ptr;
    }
}

/* Format a HOST_WIDE_INT as either a percent ratio, or absolute
   count.  If dp >= 0, format TOP/BOTTOM * 100 to DP decimal places.
   If DP is zero, no decimal point is printed. Only print 100% when
   TOP==BOTTOM and only print 0% when TOP=0.  If dp < 0, then simply
   format TOP.  Return pointer to a static string.  */

static char const *
format_hwint (top, bottom, dp)
     HOST_WIDEST_INT top, bottom;
     int dp;
{
  static char buffer[20];
  
  if (dp >= 0)
    {
      float ratio = bottom ? (float)top / bottom : 0;
      int ix;
      unsigned limit = 100;
      unsigned percent;
  
      for (ix = dp; ix--; )
	limit *= 10;
      
      percent = (unsigned) (ratio * limit + (float)0.5);
      if (percent <= 0 && top)
	percent = 1;
      else if (percent >= limit && top != bottom)
	percent = limit - 1;
      ix = sprintf (buffer, "%.*u%%", dp + 1, percent);
      if (dp)
	{
	  dp++;
	  do
	    {
	      buffer[ix+1] = buffer[ix];
	      ix--;
	    }
	  while (dp--);
	  buffer[ix + 1] = '.';
	}
    }
  else
    sprintf (buffer, HOST_WIDEST_INT_PRINT_DEC, top);
  
  return buffer;
}


/* Output summary info for a function.  */

static void
function_summary (function, title)
     struct coverage *function;
     const char *title;
{
  if (function->lines)
    fnotice (stdout, "%s of %d lines executed in %s %s\n",
	     format_hwint (function->lines_executed,
			   function->lines, 2),
	     function->lines, title, function->name);
  else
    fnotice (stdout, "No executable lines in %s %s\n",
	     title, function->name);

  if (output_branch_probs)
    {
      if (function->branches)
	{
	  fnotice (stdout, "%s of %d branches executed in %s %s\n",
		   format_hwint (function->branches_executed,
				 function->branches, 2),
		   function->branches, title, function->name);
	  fnotice (stdout,
		"%s of %d branches taken at least once in %s %s\n",
		   format_hwint (function->branches_taken,
				 function->branches, 2),
		   function->branches, title, function->name);
	}
      else
	fnotice (stdout, "No branches in %s %s\n", title, function->name);
      if (function->calls)
	fnotice (stdout, "%s of %d calls executed in %s %s\n",
		 format_hwint (function->calls_executed,
			       function->calls, 2),
		 function->calls, title, function->name);
      else
	fnotice (stdout, "No calls in %s %s\n", title, function->name);
    }
}

/* Generate an output file name. LONG_OUTPUT_NAMES and PRESERVE_PATHS
   affect name generation. With preserve_paths we create a filename
   from all path components of the source file, replacing '/' with
   '#', without it we simply take the basename component. With
   long_output_names we prepend the processed name of the input file
   to each output name (except when the current source file is the
   input file, so you don't get a double concatenation). The two
   components are separated by '##'. Also '.' filename components are
   removed and '..'  components are renamed to '^'.  */

static char *
make_gcov_file_name (src_name)
     char *src_name;
{
  char *cptr;
  char *name = xmalloc (strlen (src_name) + strlen (input_file_name) + 10);
  
  name[0] = 0;
  if (output_long_names && strcmp (src_name, input_file_name))
    {
      /* Generate the input filename part.  */
      cptr = preserve_paths ? NULL : strrchr (input_file_name, '/');
      cptr = cptr ? cptr + 1 : input_file_name;
      strcat (name, cptr);
      strcat (name, "##");
    }
   
  /* Generate the source filename part.  */
  cptr = preserve_paths ? NULL : strrchr (src_name, '/');
  cptr = cptr ? cptr + 1 : src_name;
  strcat (name, cptr);
  
  if (preserve_paths)
    {
      /* Convert '/' to '#', remove '/./', convert '/../' to '/^/' */
      char *prev;
      
      for (cptr = name; (cptr = strchr ((prev = cptr), '/'));)
 	{
 	  unsigned shift = 0;
 	  
 	  if (prev + 1 == cptr && prev[0] == '.')
 	    {
 	      /* Remove '.' */
 	      shift = 2;
 	    }
 	  else if (prev + 2 == cptr && prev[0] == '.' && prev[1] == '.')
 	    {
 	      /* Convert '..' */
 	      shift = 1;
 	      prev[1] = '^';
 	    }
 	  else
 	    *cptr++ = '#';
 	  if (shift)
 	    {
 	      cptr = prev;
 	      do
 		prev[0] = prev[shift];
	      while (*prev++);
 	    }
 	}
    }
  
  /* Don't strip off the ending for compatibility with tcov, since
     this results in confusion if there is more than one file with the
     same basename, e.g. tmp.c and tmp.h.  */
  strcat (name, ".gcov");
  return name;
}

/* Scan through the bb_data, and when the file name matches the
   source file name, then for each following line number, increment
   the line number execution count indicated by the execution count of
   the appropriate basic block.  */

static void
init_line_info (line_info, total, maxlineno)
     struct line_info *line_info;
     struct coverage *total;
     long maxlineno;
{
  long block_num = 0;		/* current block number */
  struct bb_info *block_ptr = NULL;	/* current block ptr */
  struct coverage function;
  struct coverage *func_ptr = NULL;
  struct bb_info_list *current_graph = NULL; /* Graph for current function.  */
  int is_this_file = 0;	/* We're scanning a block from the desired file.  */
  char *ptr = bb_data;
  long count;
  long line_num;
  struct line_info *line_ptr = 0; /* line info ptr.  */
   
  memset (&function, 0, sizeof (function));
  if (output_function_summary)
    func_ptr = &function;
  
  for (count = 0; count < bb_data_size; count++)
    {
      __fetch_long (&line_num, ptr, 4);
      ptr += 4;
      if (line_num < 0)
	{
	  long delim;
	  
	  if (line_num == -1)
	    {
	      /* Marks the beginning of a file name.  Check to see
	     	 whether this is the filename we are currently
	     	 collecting data for.  */
	      is_this_file = !strcmp (total->name, ptr);
	    }
	  else if (line_num == -2)
	    {
	      /* Marks the start of a new function.  Advance to the
	     	 next program flow graph.  */
	      if (!current_graph)
		current_graph = bb_graph_list;
	      else
		{
		  if (block_num == current_graph->num_blocks - 1)
		    /* Last block falls through to exit.  */
		    ;
		  else if (block_num == current_graph->num_blocks - 2)
		    {
		      if (output_branch_probs && is_this_file)
			calculate_branch_probs (block_ptr, line_ptr, func_ptr);
		    }
		  else
		    {
		      fnotice (stderr,
			       "didn't use all bb entries of graph, function %s\n",
			       function.name);
		      fnotice (stderr, "block_num = %ld, num_blocks = %d\n",
			       block_num, current_graph->num_blocks);
		    }
		  if (func_ptr && is_this_file)
		    function_summary (func_ptr, "function");
		  current_graph = current_graph->next;
		}
	      block_num = 0;
	      block_ptr = current_graph->bb_graph;
	      memset (&function, 0, sizeof (function));
	      function.name = ptr;
	    }
	  else
	    {
	      fnotice (stderr, "ERROR: unexpected line number %ld\n", line_num);
	      abort ();
	    }

	  /* Scan past the string.  */
	  for (delim = 0; delim != line_num; count++)
	    {
	      __fetch_long (&delim, ptr, 4);
	      ptr += 4;
	    }
	}
      else if (!line_num)
	{
	  /* Marks the end of a block.  */
	  if (block_num >= current_graph->num_blocks)
	    {
	      fnotice (stderr, "ERROR: too many basic blocks in function %s\n",
		       function.name);
	      abort ();
	    }
	  
	  if (output_branch_probs && is_this_file)
	    calculate_branch_probs (block_ptr, line_ptr, func_ptr);
	  
	  block_num++;
	  block_ptr++;
	}
      else if (is_this_file)
	{
	  if (line_num >= maxlineno)
	    {
	      fnotice (stderr, "ERROR: out of range line number in function %s\n",
		       function.name);
	      abort ();
	    }

	  line_ptr = &line_info[line_num];
	  if (func_ptr)
	    {
	      if (!line_ptr->exists)
		func_ptr->lines++;
	      if (!line_ptr->count && block_ptr->exec_count)
		func_ptr->lines_executed++;
	    }
	  
	  /* Accumulate execution data for this line number.  */
	  line_ptr->count += block_ptr->exec_count;
	  line_ptr->exists = 1;
	}
    }
  
  if (func_ptr && is_this_file)
    function_summary (func_ptr, "function");
  
  /* Calculate summary test coverage statistics.  */
  for (line_num = 1, line_ptr = &line_info[line_num];
       line_num < maxlineno; line_num++, line_ptr++)
    {
      struct arcdata *a_ptr, *prev, *next;
      
      if (line_ptr->exists)
	{
	  total->lines++;
	  if (line_ptr->count)
	    total->lines_executed++;
	}

      /* Total and reverse the branch information.  */
      for (a_ptr = line_ptr->branches, prev = NULL; a_ptr; a_ptr = next)
	{
	  next = a_ptr->next;
	  a_ptr->next = prev;
	  prev = a_ptr;

	  accumulate_branch_counts (total, a_ptr);
	}
      line_ptr->branches = prev;
    }
}

/* Read in the source file one line at a time, and output that line to
   the gcov file preceded by its execution count and other
   information.  */

static void
output_line_info (gcov_file, line_info, total, maxlineno)
     FILE *gcov_file;
     const struct line_info *line_info;
     const struct coverage *total;
     long maxlineno;
{
  FILE *source_file;
  long line_num;                    /* current line number */
  const struct line_info *line_ptr; /* current line info ptr.  */
  char string[STRING_SIZE];         /* line buffer.  */
  char const *retval = "";	    /* status of source file reading.  */

  fprintf (gcov_file, "%9s:%5d:Source:%s\n", "-", 0, total->name);
  fprintf (gcov_file, "%9s:%5d:Object:%s\n", "-", 0, bb_file_name);
  
  source_file = fopen (total->name, "r");
  if (!source_file)
    {
      fnotice (stderr, "Could not open source file %s.\n", total->name);
      retval = NULL;
    }
  else
    {
      struct stat status;
      
      if (!fstat (fileno (source_file), &status)
	  && status.st_mtime > bb_file_time)
	{
	  fnotice (stderr, "Warning: source file %s is newer than %s\n",
		   total->name, bb_file_name);
	  fprintf (gcov_file, "%9s:%5d:Source is newer than compiler output\n",
		   "-", 0);
	}
    }

  for (line_num = 1, line_ptr = &line_info[line_num];
       line_num < maxlineno; line_num++, line_ptr++)
    {
      /* For lines which don't exist in the .bb file, print '-' before
 	 the source line.  For lines which exist but were never
 	 executed, print '#####' before the source line.  Otherwise,
 	 print the execution count before the source line.  There are
 	 16 spaces of indentation added before the source line so that
 	 tabs won't be messed up.  */
      fprintf (gcov_file, "%9s:%5ld:",
	       !line_ptr->exists ? "-"
	       : !line_ptr->count ? "#####"
	       : format_hwint (line_ptr->count, 0, -1), line_num);
      
      if (retval)
	{
	  /* Copy source line.  */
	  do
	    {
	      retval = fgets (string, STRING_SIZE, source_file);
	      if (!retval)
		{
		  fnotice (stderr,
			   "Unexpected EOF while reading source file %s.\n",
			   total->name);
		  break;
		}
	      fputs (retval, gcov_file);
	    }
	  while (!retval[0] || retval[strlen (retval) - 1] != '\n');
	}
      if (!retval)
	fputs ("??\n", gcov_file);
      
      if (output_branch_probs)
	{
	  int i;
	  struct arcdata *a_ptr;
	  
	  for (i = 0, a_ptr = line_ptr->branches; a_ptr;
	       a_ptr = a_ptr->next, i++)
	    {
	      if (a_ptr->call_insn)
		{
		  if (a_ptr->total == 0)
		    fnotice (gcov_file, "call   %2d never executed\n", i);
		  else
		    fnotice
		      (gcov_file, "call   %2d returns %s\n", i,
		       format_hwint (a_ptr->total - a_ptr->hits,
				     a_ptr->total,
				     -output_branch_counts));
		}
	      else
		{
		  if (a_ptr->total == 0)
		    fnotice (gcov_file, "branch %2d never executed\n", i);
		  else
		    fnotice
		      (gcov_file, "branch %2d taken %s\n", i,
		       format_hwint (a_ptr->hits, a_ptr->total,
				     -output_branch_counts));
		}
	    }
	}
    }
  
  /* Handle all remaining source lines.  There may be lines after the
     last line of code.  */
  if (retval)
    {
      for (; (retval = fgets (string, STRING_SIZE, source_file)); line_num++)
	{
	  fprintf (gcov_file, "%9s:%5ld:%s", "-", line_num, retval);
	  
	  while (!retval[0] || retval[strlen (retval) - 1] != '\n')
	    {
	      retval = fgets (string, STRING_SIZE, source_file);
	      if (!retval)
		break;
	      fputs (retval, gcov_file);
	    }
	}
    }
  
  if (source_file)
    fclose (source_file);
}

/* Calculate line execution counts, and output a .gcov file for source
   file S_PTR. Allocate an array big enough to hold a count for each
   line.  Scan through the bb_data, and when the file name matches the
   current file name, then for each following line number, increment
   the line number execution count indicated by the execution count of
   the appropriate basic block.  */

static void
output_data (s_ptr)
	     struct sourcefile *s_ptr;
{
  struct line_info *line_info	/* line info data */
    = (struct line_info *) xcalloc (s_ptr->maxlineno,
				    sizeof (struct line_info));
  long line_num;
  struct coverage total;
  
  memset (&total, 0, sizeof (total));
  total.name = s_ptr->name;
  
  init_line_info (line_info, &total, s_ptr->maxlineno);
  function_summary (&total, "file");

  if (output_gcov_file)
    {
      /* Now the statistics are ready.  Read in the source file one
	 line at a time, and output that line to the gcov file
	 preceded by its execution information.  */
      
      char *gcov_file_name = make_gcov_file_name (total.name);
      FILE *gcov_file = fopen (gcov_file_name, "w");
      
      if (gcov_file)
	{
	  fnotice (stdout, "Creating %s.\n", gcov_file_name);
	  output_line_info (gcov_file, line_info, &total, s_ptr->maxlineno);
	  if (ferror (gcov_file))
	    fnotice (stderr, "Error writing output file %s.\n",
		     gcov_file_name);
	  fclose (gcov_file);
	}
      else
	fnotice (stderr, "Could not open output file %s.\n", gcov_file_name);
      free (gcov_file_name);
    }

  /* Free data.  */
  for (line_num = 1; line_num != s_ptr->maxlineno; line_num++)
    {
      struct arcdata *branch, *next;

      for (branch = line_info[line_num].branches; branch; branch = next)
	{
	  next = branch->next;
	  free (branch);
	}
    }
  free (line_info);
}
