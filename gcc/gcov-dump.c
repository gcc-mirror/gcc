/* Dump a gcov file, for debugging use.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "version.h"
#include <getopt.h>
typedef HOST_WIDEST_INT gcov_type;
#include "gcov-io.h"

static void dump_file PARAMS ((const char *));
static void print_prefix PARAMS ((const char *, unsigned));
static void print_usage PARAMS ((void));
static void print_version PARAMS ((void));
static int tag_function PARAMS ((const char *, FILE *, unsigned, unsigned));
static int tag_blocks PARAMS ((const char *, FILE *, unsigned, unsigned));
static int tag_arcs PARAMS ((const char *, FILE *, unsigned, unsigned));
static int tag_lines PARAMS ((const char *, FILE *, unsigned, unsigned));
static int tag_arc_counts PARAMS ((const char *, FILE *, unsigned, unsigned));
static int tag_summary PARAMS ((const char *, FILE *, unsigned, unsigned));
extern int main PARAMS ((int, char **));

typedef struct tag_format
{
  unsigned tag;
  char const *name;
  int (*proc) (const char *, FILE *, unsigned, unsigned);
} tag_format_t;

static int flag_dump_contents = 0;

static const struct option options[] =
{
  { "help",                 no_argument,       NULL, 'h' },
  { "version",              no_argument,       NULL, 'v' },
  { "long",                 no_argument,       NULL, 'l' },
};

static tag_format_t tag_table[] =
{
  {0, "NOP", NULL},
  {0, "UNKNOWN", NULL},
  {GCOV_TAG_FUNCTION, "FUNCTION", tag_function},
  {GCOV_TAG_BLOCKS, "BLOCKS", tag_blocks},
  {GCOV_TAG_ARCS, "ARCS", tag_arcs},
  {GCOV_TAG_LINES, "LINES", tag_lines},
  {GCOV_TAG_ARC_COUNTS, "ARC_COUNTS", tag_arc_counts},
  {GCOV_TAG_OBJECT_SUMMARY, "OBJECT_SUMMARY", tag_summary},
  {GCOV_TAG_PROGRAM_SUMMARY, "PROGRAM_SUMMARY", tag_summary},
  {GCOV_TAG_PLACEHOLDER_SUMMARY, "PROGRAM_PLACEHOLDER", tag_summary},
  {GCOV_TAG_INCORRECT_SUMMARY, "PROGRAM_INCORRECT", tag_summary},
  {0, NULL, NULL}
};

int main (argc, argv)
     int argc ATTRIBUTE_UNUSED;
     char **argv;
{
  int opt;

  while ((opt = getopt_long (argc, argv, "hlv", options, NULL)) != -1)
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
	default:
	  fprintf (stderr, "unknown flag `%c'\n", opt);
	}
    }
  
  while (argv[optind])
    dump_file (argv[optind++]);
  return 0;
}

static void
print_usage ()
{
  printf ("Usage: gcov-dump [OPTION] ... gcovfiles\n");
  printf ("Print coverage file contents\n");
  printf ("  -h, --help           Print this help\n");
  printf ("  -v, --version        Print version number\n");
  printf ("  -l, --long           Dump record contents too\n");
}

static void
print_version ()
{
  char v[4];
  unsigned version = GCOV_VERSION;
  unsigned ix;

  for (ix = 4; ix--; version >>= 8)
    v[ix] = version;
  printf ("gcov %.4s (GCC %s)\n", v, version_string);
  printf ("Copyright (C) 2002 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
}

static void
print_prefix (filename, depth)
     const char *filename;
     unsigned depth;
{
  static const char prefix[] = "    ";
  
  printf ("%s:%.*s", filename, depth, prefix);
}

static void
dump_file (filename)
     const char *filename;
{
  FILE *file = fopen (filename, "rb");
  unsigned tags[4];
  unsigned depth = 0;
  unsigned magic, version;
  unsigned tag, length;
  
  if (!file)
    {
      fprintf (stderr, "%s:cannot open\n", filename);
      return;
    }
  
  if (gcov_read_unsigned (file, &magic)
      || gcov_read_unsigned (file, &version))
    {
    read_error:;
      printf ("%s:read error at %ld\n", filename, ftell (file));
      fclose (file);
      return;
    }

  /* magic */
  {
    const char *type = NULL;
    char e[4], v[4], m[4];
    unsigned expected = GCOV_VERSION;
    unsigned ix;
    int different = version != GCOV_VERSION;
    
    if (magic == GCOV_DATA_MAGIC)
      type = "data";
    else if (magic == GCOV_GRAPH_MAGIC)
      type = "graph";
    else
      {
	printf ("%s:not a gcov file\n", filename);
	fclose (file);
	return;
      }
    for (ix = 4; ix--; expected >>= 8, version >>= 8, magic >>= 8)
      {
	e[ix] = expected;
	v[ix] = version;
	m[ix] = magic;
      }
    
    printf ("%s:%s:magic `%.4s':version `%.4s'\n", filename, type, m, v);
    if (different)
      printf ("%s:warning:current version is `%.4s'\n", filename, e);
  }

  while (!gcov_read_unsigned (file, &tag)
	 && !gcov_read_unsigned (file, &length))
    {
      tag_format_t const *format;
      unsigned tag_depth;
      long base, end;
      
      base = gcov_save_position (file);
      
      if (!tag)
	tag_depth = depth;
      else
	{
	  unsigned mask = GCOV_TAG_MASK (tag) >> 1;
	  
	  for (tag_depth = 4; mask; mask >>= 8)
	    {
	      if ((mask & 0xff) != 0xff)
		{
		  printf ("%s:tag `%08x' is invalid\n", filename, tag);
		  break;
		}
	      tag_depth--;
	    }
	}
      for (format = tag_table; format->name; format++)
	if (format->tag == tag)
	  goto found;
      format = &tag_table[1];
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
      
      print_prefix (filename, tag_depth);
      printf ("%08x:%4u:%s", tag, length, format->name);
      if (format->proc)
	if ((*format->proc) (filename, file, tag, length))
	  goto read_error;
      printf ("\n");
      end = gcov_save_position (file);
      gcov_resync (file, base, length);
      if (format->proc && end != base + (long)length)
	{
	  if (end > base + (long)length)
	    printf ("%s:record size mismatch %lu bytes overread\n",
		    filename, (end - base) - length);
	  else
	    printf ("%s:record size mismatch %lu bytes unread\n",
		    filename, length - (end - base));
	}
    }
  if (!feof (file))
    goto read_error;
  fclose (file);
}

static int
tag_function (filename, file, tag, length)
     const char *filename ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     unsigned tag ATTRIBUTE_UNUSED;
     unsigned length ATTRIBUTE_UNUSED;
{
  char *name = NULL;
  unsigned checksum;

  if (gcov_read_string (file, &name, NULL)
      || gcov_read_unsigned (file, &checksum))
    return 1;

  printf (" `%s' checksum=0x%08x", name, checksum);
  free (name);
  
  return 0;
}

static int
tag_blocks (filename, file, tag, length)
     const char *filename ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     unsigned tag ATTRIBUTE_UNUSED;
     unsigned length ATTRIBUTE_UNUSED;
{
  unsigned n_blocks = length / 4;
  
  printf (" %u blocks", n_blocks);

  if (flag_dump_contents)
    {
      unsigned ix;

      for (ix = 0; ix != n_blocks; ix++)
	{
	  unsigned flags;
	  if (gcov_read_unsigned (file, &flags))
	    return 1;
	  if (!(ix & 7))
	    printf ("\n%s:\t\t%u", filename, ix);
	  printf (" %04x", flags);
	}
      
    }
  else
    gcov_skip (file, n_blocks * 4);
  
  return 0;
}

static int
tag_arcs (filename, file, tag, length)
     const char *filename ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     unsigned tag ATTRIBUTE_UNUSED;
     unsigned length ATTRIBUTE_UNUSED;
{
  unsigned n_arcs = (length - 4) / 8;

  printf (" %u arcs", n_arcs);
  if (flag_dump_contents)
    {
      unsigned ix;
      unsigned blockno;

      if (gcov_read_unsigned (file, &blockno))
	return 1;

      for (ix = 0; ix != n_arcs; ix++)
	{
	  unsigned dst, flags;
	  
	  if (gcov_read_unsigned (file, &dst)
	      || gcov_read_unsigned (file, &flags))
	    return 1;
	  if (!(ix & 3))
	    printf ("\n%s:\t\t%u:", filename, blockno);
	  printf (" %u:%04x", dst, flags);
	}
    }
  else
    gcov_skip (file, 4 + n_arcs * 8);
  
  return 0;
}

static int
tag_lines (filename, file, tag, length)
     const char *filename ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     unsigned tag ATTRIBUTE_UNUSED;
     unsigned length ATTRIBUTE_UNUSED;
{
  if (flag_dump_contents)
    {
      char *source = NULL;
      unsigned blockno;
      char const *sep = NULL;

      if (gcov_read_unsigned (file, &blockno))
	return 1;
      
      while (1)
	{
	  unsigned lineno;
	  
	  if (gcov_read_unsigned (file, &lineno))
	    {
	      free (source);
	      return 1;
	    }
	  if (!lineno)
	    {
	      if (gcov_read_string (file, &source, NULL))
		return 1;
	      if (!source)
		break;
	      sep = NULL;
	    }
	  
	  if (!sep)
	    {
	      printf ("\n%s:\t\t%u:", filename, blockno);
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
  else
    gcov_skip (file, length);
  
  return 0;
}

static int
tag_arc_counts (filename, file, tag, length)
     const char *filename ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     unsigned tag ATTRIBUTE_UNUSED;
     unsigned length ATTRIBUTE_UNUSED;
{
  unsigned n_counts = length / 8;
  
  printf (" %u counts", n_counts);
  if (flag_dump_contents)
    {
      unsigned ix;

      for (ix = 0; ix != n_counts; ix++)
	{
	  gcov_type count;
	  
	  if (gcov_read_counter (file, &count))
	    return 1;
	  if (!(ix & 7))
	    printf ("\n%s:\t\t%u", filename, ix);
	  printf (" ");
	  printf (HOST_WIDEST_INT_PRINT_DEC, count);
	}
    }
  else
    gcov_skip (file, n_counts * 8);
  
  return 0;
}

static int
tag_summary (filename, file, tag, length)
     const char *filename ATTRIBUTE_UNUSED;
     FILE *file ATTRIBUTE_UNUSED;
     unsigned tag ATTRIBUTE_UNUSED;
     unsigned length ATTRIBUTE_UNUSED;
{
  struct gcov_summary summary;

  if (gcov_read_summary (file, &summary))
    return 1;
  printf (" checksum=0x%08x", summary.checksum);
  
  printf ("\n%s:\t\truns=%u, arcs=%u", filename,
	  summary.runs, summary.arcs);
  printf ("\n%s:\t\tarc_sum=", filename);
  printf (HOST_WIDEST_INT_PRINT_DEC, 
	  (HOST_WIDEST_INT)summary.arc_sum);
  printf (", arc_max_one=");
  printf (HOST_WIDEST_INT_PRINT_DEC, 
	  (HOST_WIDEST_INT)summary.arc_max_one);
  printf ("\n%s:\t\tmax_sum=", filename);
  printf (HOST_WIDEST_INT_PRINT_DEC, 
	  (HOST_WIDEST_INT)summary.arc_max_sum);
  printf (", sum_max=");
  printf (HOST_WIDEST_INT_PRINT_DEC, 
	  (HOST_WIDEST_INT)summary.arc_sum_max);
  return 0;
}
