/* Dump a gcov file, for debugging use.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "version.h"
#include "intl.h"
#include "diagnostic.h"
#include <getopt.h>
#define IN_GCOV (-1)
#include "gcov-io.h"
#include "gcov-io.cc"

using namespace std;

static void dump_gcov_file (const char *);
static void print_prefix (const char *, unsigned, gcov_position_t);
static void print_usage (void);
static void print_version (void);
static void tag_function (const char *, unsigned, int, unsigned);
static void tag_blocks (const char *, unsigned, int, unsigned);
static void tag_arcs (const char *, unsigned, int, unsigned);
static void tag_conditions (const char *, unsigned, int, unsigned);
static void tag_lines (const char *, unsigned, int, unsigned);
static void tag_counters (const char *, unsigned, int, unsigned);
static void tag_summary (const char *, unsigned, int, unsigned);
extern int main (int, char **);

typedef struct tag_format
{
  unsigned tag;
  char const *name;
  void (*proc) (const char *, unsigned, int, unsigned);
} tag_format_t;

static int flag_dump_contents = 0;
static int flag_dump_positions = 0;
static int flag_dump_raw = 0;
static int flag_dump_stable = 0;

static const struct option options[] =
{
  { "help",                 no_argument,       NULL, 'h' },
  { "version",              no_argument,       NULL, 'v' },
  { "long",                 no_argument,       NULL, 'l' },
  { "positions",	    no_argument,       NULL, 'o' },
  { "raw",		    no_argument,       NULL, 'r' },
  { "stable",		    no_argument,       NULL, 's' },
  {}
};

#define VALUE_PADDING_PREFIX "              "
#define VALUE_PREFIX "%2d: "

static const tag_format_t tag_table[] =
{
  {0, "NOP", NULL},
  {0, "UNKNOWN", NULL},
  {0, "COUNTERS", tag_counters},
  {GCOV_TAG_FUNCTION, "FUNCTION", tag_function},
  {GCOV_TAG_BLOCKS, "BLOCKS", tag_blocks},
  {GCOV_TAG_ARCS, "ARCS", tag_arcs},
  {GCOV_TAG_CONDS, "CONDITIONS", tag_conditions},
  {GCOV_TAG_LINES, "LINES", tag_lines},
  {GCOV_TAG_OBJECT_SUMMARY, "OBJECT_SUMMARY", tag_summary},
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

  while ((opt = getopt_long (argc, argv, "hlprsvw", options, NULL)) != -1)
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
	case 'r':
	  flag_dump_raw = 1;
	  break;
	case 's':
	  flag_dump_stable = 1;
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
  printf ("  -r, --raw            Print content records in raw format\n");
  printf ("  -s, --stable         Print content in stable "
	  "format usable for comparison\n");
  printf ("  -v, --version        Print version number\n");
  printf ("\nFor bug reporting instructions, please see:\n%s.\n",
	   bug_report_url);
}

static void
print_version (void)
{
  printf ("gcov-dump %s%s\n", pkgversion_string, version_string);
  printf ("Copyright (C) 2024 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
}

static void
print_prefix (const char *filename, unsigned depth, gcov_position_t position)
{
  static const char prefix[] = "    ";

  printf ("%s:", filename);
  if (flag_dump_positions)
    printf ("%5lu:", (unsigned long) position);
  printf ("%.*s", (int) 2 * depth, prefix);
}

static void
dump_gcov_file (const char *filename)
{
  unsigned tags[4];
  unsigned depth = 0;
  bool is_data_type;

  if (!gcov_open (filename, 1))
    {
      fprintf (stderr, "%s:cannot open\n", filename);
      return;
    }

  /* magic */
  {
    unsigned magic = gcov_read_unsigned ();
    unsigned version;
    int endianness = 0;
    char m[4], v[4];

    if ((endianness = gcov_magic (magic, GCOV_DATA_MAGIC)))
      is_data_type = true;
    else if ((endianness = gcov_magic (magic, GCOV_NOTE_MAGIC)))
      is_data_type = false;
    else
      {
	printf ("%s:not a gcov file\n", filename);
	gcov_close ();
	return;
      }
    version = gcov_read_unsigned ();
    GCOV_UNSIGNED2STRING (v, version);
    GCOV_UNSIGNED2STRING (m, magic);

    printf ("%s:%s:magic `%.4s':version `%.4s'%s\n", filename,
	    is_data_type ? "data" : "note",
 	    m, v, endianness < 0 ? " (swapped endianness)" : "");
    if (version != GCOV_VERSION)
      {
	char e[4];

	GCOV_UNSIGNED2STRING (e, GCOV_VERSION);
	printf ("%s:warning:current version is `%.4s'\n", filename, e);
      }
  }

  /* stamp */
  unsigned stamp = gcov_read_unsigned ();
  printf ("%s:stamp %u\n", filename, stamp);

  /* Checksum */
  unsigned checksum = gcov_read_unsigned ();
  printf ("%s:checksum %u\n", filename, checksum);

  if (!is_data_type)
    {
      printf ("%s:cwd: %s\n", filename, gcov_read_string ());

      /* Support for unexecuted basic blocks.  */
      unsigned support_unexecuted_blocks = gcov_read_unsigned ();
      if (!support_unexecuted_blocks)
	printf ("%s: has_unexecuted_block is not supported\n", filename);
    }

  while (1)
    {
      gcov_position_t base, position = gcov_position ();
      int read_length;
      unsigned tag, length;
      tag_format_t const *format;
      unsigned tag_depth;
      int error;
      unsigned mask;

      tag = gcov_read_unsigned ();
      if (!tag)
	break;
      read_length = (int)gcov_read_unsigned ();
      length = read_length > 0 ? read_length : 0;
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
      printf ("%08x:%4u:%s", tag, abs (read_length), format->name);
      if (format->proc)
	(*format->proc) (filename, tag, read_length, depth);

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
	  printf (error < 0 ? "%s:counter overflow at %lu\n"
		  : "%s:read error at %lu\n", filename,
		  (long unsigned) gcov_position ());
	  break;
	}
    }
  gcov_close ();
}

static void
tag_function (const char *filename ATTRIBUTE_UNUSED,
	      unsigned tag ATTRIBUTE_UNUSED, int length,
	      unsigned depth ATTRIBUTE_UNUSED)
{
  gcov_position_t pos = gcov_position ();

  if (!length)
    printf (" placeholder");
  else
    {
      printf (" ident=%u", gcov_read_unsigned ());
      printf (", lineno_checksum=0x%08x", gcov_read_unsigned ());
      printf (", cfg_checksum=0x%08x", gcov_read_unsigned ());

      if (gcov_position () - pos < (gcov_position_t) length)
	{
	  const char *name;

	  name = gcov_read_string ();
	  printf (", `%s'", name ? name : "NULL");
	  unsigned artificial = gcov_read_unsigned ();
	  name = gcov_read_string ();
	  printf (" %s", name ? name : "NULL");
	  unsigned line_start = gcov_read_unsigned ();
	  unsigned column_start = gcov_read_unsigned ();
	  unsigned line_end = gcov_read_unsigned ();
	  unsigned column_end = gcov_read_unsigned ();
	  printf (":%u:%u-%u:%u", line_start, column_start,
		  line_end, column_end);
	  if (artificial)
	    printf (", artificial");
	}
    }
}

static void
tag_blocks (const char *filename ATTRIBUTE_UNUSED,
	    unsigned tag ATTRIBUTE_UNUSED, int length ATTRIBUTE_UNUSED,
	    unsigned depth ATTRIBUTE_UNUSED)
{
  printf (" %u blocks", gcov_read_unsigned ());
}

static void
tag_arcs (const char *filename ATTRIBUTE_UNUSED,
	  unsigned tag ATTRIBUTE_UNUSED, int length ATTRIBUTE_UNUSED,
	  unsigned depth)
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
	      print_prefix (filename, depth, gcov_position ());
	      printf (VALUE_PADDING_PREFIX "block %u:", blockno);
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

/* Print number of conditions (not outcomes, i.e. if (x && y) is 2, not 4).  */
static void
tag_conditions (const char *filename, unsigned /* tag */, int length,
		unsigned depth)
{
  unsigned n_conditions = GCOV_TAG_CONDS_NUM (length);

  printf (" %u conditions", n_conditions);
  if (flag_dump_contents)
    {
      for (unsigned ix = 0; ix != n_conditions; ix++)
	{
	  const unsigned blockno = gcov_read_unsigned ();
	  const unsigned nterms = gcov_read_unsigned ();

	  printf ("\n");
	  print_prefix (filename, depth, gcov_position ());
	  printf (VALUE_PADDING_PREFIX "block %u:", blockno);
	  printf (" %u", nterms);
	}
    }
}
static void
tag_lines (const char *filename ATTRIBUTE_UNUSED,
	   unsigned tag ATTRIBUTE_UNUSED, int length ATTRIBUTE_UNUSED,
	   unsigned depth)
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
	      print_prefix (filename, depth, position);
	      printf (VALUE_PADDING_PREFIX "block %u:", blockno);
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
	      unsigned tag ATTRIBUTE_UNUSED, int length ATTRIBUTE_UNUSED,
	      unsigned depth)
{
#define DEF_GCOV_COUNTER(COUNTER, NAME, MERGE_FN) NAME,
  static const char *const counter_names[] = {
#include "gcov-counter.def"
};
#undef DEF_GCOV_COUNTER
  int n_counts = GCOV_TAG_COUNTER_NUM (length);
  bool has_zeros = n_counts < 0;
  n_counts = abs (n_counts);
  unsigned counter_idx = GCOV_COUNTER_FOR_TAG (tag);

  printf (" %s %u counts%s",
	  counter_names[counter_idx], n_counts,
	  has_zeros ? " (all zero)" : "");
  if (flag_dump_contents)
    {
      vector<gcov_type> counters;
      for (int ix = 0; ix != n_counts; ix++)
	counters.push_back (has_zeros ? 0 : gcov_read_counter ());

      /* Make stable sort for TOP N counters.  */
      if (flag_dump_stable)
	if (counter_idx == GCOV_COUNTER_V_INDIR
	    || counter_idx == GCOV_COUNTER_V_TOPN)
	  {
	    unsigned start = 0;
	    while (start < counters.size ())
	      {
		unsigned n = counters[start + 1];

		/* Use bubble sort.  */
		for (unsigned i = 1; i <= n; ++i)
		  for (unsigned j = i; j <= n; ++j)
		    {
		      gcov_type key1 = counters[start + 2 * i];
		      gcov_type value1 = counters[start + 2 * i + 1];
		      gcov_type key2 = counters[start + 2 * j];
		      gcov_type value2 = counters[start + 2 * j + 1];

		      if (value1 < value2 || (value1 == value2 && key1 < key2))
			{
			  std::swap (counters[start + 2 * i],
				     counters[start + 2 * j]);
			  std::swap (counters[start + 2 * i + 1],
				     counters[start + 2 * j + 1]);
			}
		    }
		start += 2 * (n + 1);
	      }
	    if (start != counters.size ())
	      abort ();
	  }

      for (unsigned ix = 0; ix < counters.size (); ++ix)
	{
	  if (flag_dump_raw)
	    {
	      if (ix == 0)
		printf (": ");
	    }
	  else if (!(ix & 7))
	    {
	      printf ("\n");
	      print_prefix (filename, depth, gcov_position ());
	      printf (VALUE_PADDING_PREFIX VALUE_PREFIX, ix);
	    }

	  printf ("%" PRId64 " ", counters[ix]);
	}
    }
}

static void
tag_summary (const char *filename ATTRIBUTE_UNUSED,
	     unsigned tag ATTRIBUTE_UNUSED, int length ATTRIBUTE_UNUSED,
	     unsigned depth ATTRIBUTE_UNUSED)
{
  gcov_summary summary;
  gcov_read_summary (&summary);
  printf (" runs=%d, sum_max=%" PRId64,
	  summary.runs, summary.sum_max);
}
