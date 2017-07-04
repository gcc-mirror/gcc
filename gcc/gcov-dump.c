/* Dump a gcov file, for debugging use.
   Copyright (C) 2002-2016 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>

Gcov is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Gcov is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Gcov; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "version.h"
#include "intl.h"
#include "diagnostic.h"
#include <getopt.h>
#define IN_GCOV (-1)
#include "gcov-io.h"
#include "gcov-io.c"

static void dump_gcov_file (const char *);
static void print_prefix (const char *, unsigned, gcov_position_t);
static void print_usage (void);
static void print_version (void);
static void tag_function (const char *, unsigned, unsigned);
static void tag_blocks (const char *, unsigned, unsigned);
static void tag_arcs (const char *, unsigned, unsigned);
static void tag_lines (const char *, unsigned, unsigned);
static void tag_counters (const char *, unsigned, unsigned);
static void tag_summary (const char *, unsigned, unsigned);
static void dump_working_sets (const char *filename ATTRIBUTE_UNUSED,
                               const struct gcov_ctr_summary *summary);
extern int main (int, char **);

typedef struct tag_format
{
  unsigned tag;
  char const *name;
  void (*proc) (const char *, unsigned, unsigned);
} tag_format_t;

static int flag_dump_contents = 0;
static int flag_dump_positions = 0;
static int flag_dump_working_sets = 0;

static const struct option options[] =
{
  { "help",                 no_argument,       NULL, 'h' },
  { "version",              no_argument,       NULL, 'v' },
  { "long",                 no_argument,       NULL, 'l' },
  { "positions",	    no_argument,       NULL, 'o' },
  { "working-sets",	    no_argument,       NULL, 'w' },
  { 0, 0, 0, 0 }
};

static const tag_format_t tag_table[] =
{
  {0, "NOP", NULL},
  {0, "UNKNOWN", NULL},
  {0, "COUNTERS", tag_counters},
  {GCOV_TAG_FUNCTION, "FUNCTION", tag_function},
  {GCOV_TAG_BLOCKS, "BLOCKS", tag_blocks},
  {GCOV_TAG_ARCS, "ARCS", tag_arcs},
  {GCOV_TAG_LINES, "LINES", tag_lines},
  {GCOV_TAG_OBJECT_SUMMARY, "OBJECT_SUMMARY", tag_summary},
  {GCOV_TAG_PROGRAM_SUMMARY, "PROGRAM_SUMMARY", tag_summary},
  {0, NULL, NULL}
};

int
main (int argc ATTRIBUTE_UNUSED, char **argv)
{
  int opt;
  const char *p;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  diagnostic_initialize (global_dc, 0);

  while ((opt = getopt_long (argc, argv, "hlpvw", options, NULL)) != -1)
    {
      switch (opt)
	{
	case 'h':
	  print_usage ();
	  break;
	case 'v':
	  print_version ();
	  break;
	case 'l':
	  flag_dump_contents = 1;
	  break;
	case 'p':
	  flag_dump_positions = 1;
	  break;
	case 'w':
	  flag_dump_working_sets = 1;
	  break;
	default:
	  fprintf (stderr, "unknown flag `%c'\n", opt);
	}
    }

  while (argv[optind])
    dump_gcov_file (argv[optind++]);
  return 0;
}

static void
print_usage (void)
{
  printf ("Usage: gcov-dump [OPTION] ... gcovfiles\n");
  printf ("Print coverage file contents\n");
  printf ("  -h, --help           Print this help\n");
  printf ("  -l, --long           Dump record contents too\n");
  printf ("  -p, --positions      Dump record positions\n");
  printf ("  -v, --version        Print version number\n");
  printf ("  -w, --working-sets   Dump working set computed from summary\n");
  printf ("\nFor bug reporting instructions, please see:\n%s.\n",
	   bug_report_url);
}

static void
print_version (void)
{
  printf ("gcov-dump %s%s\n", pkgversion_string, version_string);
  printf ("Copyright (C) 2017 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.\n"
  	  "There is NO warranty; not even for MERCHANTABILITY or \n"
	  "FITNESS FOR A PARTICULAR PURPOSE.\n\n");
}

static void
print_prefix (const char *filename, unsigned depth, gcov_position_t position)
{
  static const char prefix[] = "    ";

  printf ("%s:", filename);
  if (flag_dump_positions)
    printf ("%lu:", (unsigned long) position);
  printf ("%.*s", (int) depth, prefix);
}

static void
dump_gcov_file (const char *filename)
{
  unsigned tags[4];
  unsigned depth = 0;

  if (!gcov_open (filename, 1))
    {
      fprintf (stderr, "%s:cannot open\n", filename);
      return;
    }

  /* magic */
  {
    unsigned magic = gcov_read_unsigned ();
    unsigned version;
    const char *type = NULL;
    int endianness = 0;
    char m[4], v[4];

    if ((endianness = gcov_magic (magic, GCOV_DATA_MAGIC)))
      type = "data";
    else if ((endianness = gcov_magic (magic, GCOV_NOTE_MAGIC)))
      type = "note";
    else
      {
	printf ("%s:not a gcov file\n", filename);
	gcov_close ();
	return;
      }
    version = gcov_read_unsigned ();
    GCOV_UNSIGNED2STRING (v, version);
    GCOV_UNSIGNED2STRING (m, magic);

    printf ("%s:%s:magic `%.4s':version `%.4s'%s\n", filename, type,
 	    m, v, endianness < 0 ? " (swapped endianness)" : "");
    if (version != GCOV_VERSION)
      {
	char e[4];

	GCOV_UNSIGNED2STRING (e, GCOV_VERSION);
	printf ("%s:warning:current version is `%.4s'\n", filename, e);
      }
  }

  /* stamp */
  {
    unsigned stamp = gcov_read_unsigned ();

    printf ("%s:stamp %lu\n", filename, (unsigned long)stamp);
  }

  while (1)
    {
      gcov_position_t base, position = gcov_position ();
      unsigned tag, length;
      tag_format_t const *format;
      unsigned tag_depth;
      int error;
      unsigned mask;

      tag = gcov_read_unsigned ();
      if (!tag)
	break;
      length = gcov_read_unsigned ();
      base = gcov_position ();
      mask = GCOV_TAG_MASK (tag) >> 1;
      for (tag_depth = 4; mask; mask >>= 8)
	{
	  if ((mask & 0xff) != 0xff)
	    {
	      printf ("%s:tag `%08x' is invalid\n", filename, tag);
	      break;
	    }
	  tag_depth--;
	}
      for (format = tag_table; format->name; format++)
	if (format->tag == tag)
	  goto found;
      format = &tag_table[GCOV_TAG_IS_COUNTER (tag) ? 2 : 1];
    found:;
      if (tag)
	{
	  if (depth && depth < tag_depth)
	    {
	      if (!GCOV_TAG_IS_SUBTAG (tags[depth - 1], tag))
		printf ("%s:tag `%08x' is incorrectly nested\n",
			filename, tag);
	    }
	  depth = tag_depth;
	  tags[depth - 1] = tag;
	}

      print_prefix (filename, tag_depth, position);
      printf ("%08x:%4u:%s", tag, length, format->name);
      if (format->proc)
	(*format->proc) (filename, tag, length);

      printf ("\n");
      if (flag_dump_contents && format->proc)
	{
	  unsigned long actual_length = gcov_position () - base;

	  if (actual_length > length)
	    printf ("%s:record size mismatch %lu bytes overread\n",
		    filename, actual_length - length);
	  else if (length > actual_length)
	    printf ("%s:record size mismatch %lu bytes unread\n",
		    filename, length - actual_length);
	}
      gcov_sync (base, length);
      if ((error = gcov_is_error ()))
	{
	  printf (error < 0 ? "%s:counter overflow at %lu\n" :
		  "%s:read error at %lu\n", filename,
		  (long unsigned) gcov_position ());
	  break;
	}
    }
  gcov_close ();
}

static void
tag_function (const char *filename ATTRIBUTE_UNUSED,
	      unsigned tag ATTRIBUTE_UNUSED, unsigned length)
{
  unsigned long pos = gcov_position ();

  if (!length)
    printf (" placeholder");
  else
    {
      printf (" ident=%u", gcov_read_unsigned ());
      printf (", lineno_checksum=0x%08x", gcov_read_unsigned ());
      printf (", cfg_checksum=0x%08x", gcov_read_unsigned ());

      if (gcov_position () - pos < length)
	{
	  const char *name;
	  
	  name = gcov_read_string ();
	  printf (", `%s'", name ? name : "NULL");
	  name = gcov_read_string ();
	  printf (" %s", name ? name : "NULL");
	  printf (":%u", gcov_read_unsigned ());
	}
    }
}

static void
tag_blocks (const char *filename ATTRIBUTE_UNUSED,
	    unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  unsigned n_blocks = GCOV_TAG_BLOCKS_NUM (length);

  printf (" %u blocks", n_blocks);

  if (flag_dump_contents)
    {
      unsigned ix;

      for (ix = 0; ix != n_blocks; ix++)
	{
	  if (!(ix & 7))
	    {
	      printf ("\n");
	      print_prefix (filename, 0, gcov_position ());
	      printf ("\t\t%u", ix);
	    }
	  printf (" %04x", gcov_read_unsigned ());
	}
    }
}

static void
tag_arcs (const char *filename ATTRIBUTE_UNUSED,
	  unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  unsigned n_arcs = GCOV_TAG_ARCS_NUM (length);

  printf (" %u arcs", n_arcs);
  if (flag_dump_contents)
    {
      unsigned ix;
      unsigned blockno = gcov_read_unsigned ();

      for (ix = 0; ix != n_arcs; ix++)
	{
	  unsigned dst, flags;

	  if (!(ix & 3))
	    {
	      printf ("\n");
	      print_prefix (filename, 0, gcov_position ());
	      printf ("\tblock %u:", blockno);
	    }
	  dst = gcov_read_unsigned ();
	  flags = gcov_read_unsigned ();
	  printf (" %u:%04x", dst, flags);
	  if (flags)
	    {
	      char c = '(';
	      
	      if (flags & GCOV_ARC_ON_TREE)
		printf ("%ctree", c), c = ',';
	      if (flags & GCOV_ARC_FAKE)
		printf ("%cfake", c), c = ',';
	      if (flags & GCOV_ARC_FALLTHROUGH)
		printf ("%cfall", c), c = ',';
	      printf (")");
	    }
	}
    }
}

static void
tag_lines (const char *filename ATTRIBUTE_UNUSED,
	   unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  if (flag_dump_contents)
    {
      unsigned blockno = gcov_read_unsigned ();
      char const *sep = NULL;

      while (1)
	{
	  gcov_position_t position = gcov_position ();
	  const char *source = NULL;
	  unsigned lineno = gcov_read_unsigned ();

	  if (!lineno)
	    {
	      source = gcov_read_string ();
	      if (!source)
		break;
	      sep = NULL;
	    }

	  if (!sep)
	    {
	      printf ("\n");
	      print_prefix (filename, 0, position);
	      printf ("\tblock %u:", blockno);
	      sep = "";
	    }
	  if (lineno)
	    {
	      printf ("%s%u", sep, lineno);
	      sep = ", ";
	    }
	  else
	    {
	      printf ("%s`%s'", sep, source);
	      sep = ":";
	    }
	}
    }
}

static void
tag_counters (const char *filename ATTRIBUTE_UNUSED,
	      unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
#define DEF_GCOV_COUNTER(COUNTER, NAME, MERGE_FN) NAME,
  static const char *const counter_names[] = {
#include "gcov-counter.def"
};
#undef DEF_GCOV_COUNTER
  unsigned n_counts = GCOV_TAG_COUNTER_NUM (length);

  printf (" %s %u counts",
	  counter_names[GCOV_COUNTER_FOR_TAG (tag)], n_counts);
  if (flag_dump_contents)
    {
      unsigned ix;

      for (ix = 0; ix != n_counts; ix++)
	{
	  gcov_type count;

	  if (!(ix & 7))
	    {
	      printf ("\n");
	      print_prefix (filename, 0, gcov_position ());
	      printf ("\t\t%u", ix);
	    }

	  count = gcov_read_counter ();
	  printf (" ");
	  printf ("%" PRId64, count);
	}
    }
}

static void
tag_summary (const char *filename ATTRIBUTE_UNUSED,
	     unsigned tag ATTRIBUTE_UNUSED, unsigned length ATTRIBUTE_UNUSED)
{
  struct gcov_summary summary;
  unsigned ix, h_ix;
  gcov_bucket_type *histo_bucket;

  gcov_read_summary (&summary);
  printf (" checksum=0x%08x", summary.checksum);

  for (ix = 0; ix != GCOV_COUNTERS_SUMMABLE; ix++)
    {
      printf ("\n");
      print_prefix (filename, 0, 0);
      printf ("\t\tcounts=%u, runs=%u",
	      summary.ctrs[ix].num, summary.ctrs[ix].runs);

      printf (", sum_all=%" PRId64,
	      (int64_t)summary.ctrs[ix].sum_all);
      printf (", run_max=%" PRId64,
	      (int64_t)summary.ctrs[ix].run_max);
      printf (", sum_max=%" PRId64,
	      (int64_t)summary.ctrs[ix].sum_max);
      if (ix != GCOV_COUNTER_ARCS)
        continue;
      printf ("\n");
      print_prefix (filename, 0, 0);
      printf ("\t\tcounter histogram:");
      for (h_ix = 0; h_ix < GCOV_HISTOGRAM_SIZE; h_ix++)
        {
          histo_bucket = &summary.ctrs[ix].histogram[h_ix];
          if (!histo_bucket->num_counters)
            continue;
          printf ("\n");
          print_prefix (filename, 0, 0);
          printf ("\t\t%d: num counts=%u, min counter="
              "%" PRId64 ", cum_counter="
              "%" PRId64,
	      h_ix, histo_bucket->num_counters,
              (int64_t)histo_bucket->min_value,
              (int64_t)histo_bucket->cum_value);
        }
      if (flag_dump_working_sets)
        dump_working_sets (filename, &summary.ctrs[ix]);
    }
}

static void
dump_working_sets (const char *filename ATTRIBUTE_UNUSED,
                   const struct gcov_ctr_summary *summary)
{
  gcov_working_set_t gcov_working_sets[NUM_GCOV_WORKING_SETS];
  unsigned ws_ix, pctinc, pct;
  gcov_working_set_t *ws_info;

  compute_working_sets (summary, gcov_working_sets);

  printf ("\n");
  print_prefix (filename, 0, 0);
  printf ("\t\tcounter working sets:");
  /* Multiply the percentage by 100 to avoid float.  */
  pctinc = 100 * 100 / NUM_GCOV_WORKING_SETS;
  for (ws_ix = 0, pct = pctinc; ws_ix < NUM_GCOV_WORKING_SETS;
       ws_ix++, pct += pctinc)
    {
      if (ws_ix == NUM_GCOV_WORKING_SETS - 1)
        pct = 9990;
      ws_info = &gcov_working_sets[ws_ix];
      /* Print out the percentage using int arithmatic to avoid float.  */
      printf ("\n");
      print_prefix (filename, 0, 0);
      printf ("\t\t%u.%02u%%: num counts=%u, min counter="
               "%" PRId64,
               pct / 100, pct - (pct / 100 * 100),
               ws_info->num_counters,
               (int64_t)ws_info->min_counter);
    }
}
