/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

#include "gcov-io.h"
#include <string.h>
#if defined (TARGET_HAS_F_SETLKW)
#include <fcntl.h>
#include <errno.h>
#endif

/* Chain of per-object gcov structures.  */
static struct gcov_info *gcov_list;

/* A program checksum allows us to distinguish program data for an
   object file included in multiple programs.  */
static unsigned gcov_crc32;

static void
gcov_version_mismatch (struct gcov_info *ptr, unsigned version)
{
  unsigned expected = GCOV_VERSION;
  unsigned ix;
  char e[4], v[4];

  for (ix = 4; ix--; expected >>= 8, version >>= 8)
    {
      e[ix] = expected;
      v[ix] = version;
    }
  
  fprintf (stderr,
	   "profiling:%s:Version mismatch - expected %.4s got %.4s\n",
	   ptr->filename, e, v);
}

/* Dump the coverage counts. We merge with existing counts when
   possible, to avoid growing the .da files ad infinitum. We use this
   program's checksum to make sure we only accumulate whole program
   statistics to the correct summary. An object file might be embedded
   in two separate programs, and we must keep the two program
   summaries separate.  */

static void
gcov_exit (void)
{
  struct gcov_info *ptr;
  unsigned ix, jx;
  struct gcov_summary program;
  gcov_type program_max_one = 0;
  gcov_type program_max_sum = 0;
  gcov_type program_sum = 0;
  unsigned program_arcs = 0;
  
#if defined (TARGET_HAS_F_SETLKW)
  struct flock s_flock;

  s_flock.l_type = F_WRLCK;
  s_flock.l_whence = SEEK_SET;
  s_flock.l_start = 0;
  s_flock.l_len = 0; /* Until EOF.  */
  s_flock.l_pid = getpid ();
#endif

  memset (&program, 0, sizeof (program));
  program.checksum = gcov_crc32;
  
  for (ptr = gcov_list; ptr; ptr = ptr->next)
    {
      FILE *da_file;
      struct gcov_summary object;
      struct gcov_summary local_prg;
      int merging = 0;
      long base;
      const struct function_info *fn_info;
      gcov_type *count_ptr;
      gcov_type object_max_one = 0;

      ptr->wkspc = 0;
      if (!ptr->filename)
	continue;

      for (ix = ptr->n_arc_counts, count_ptr = ptr->arc_counts; ix--;)
	{
	  gcov_type count = *count_ptr++;

	  if (count > object_max_one)
	    object_max_one = count;
	}
      if (object_max_one > program_max_one)
	program_max_one = object_max_one;
      
      memset (&local_prg, 0, sizeof (local_prg));
      memset (&object, 0, sizeof (object));
      
      /* Open for modification */
      if ((da_file = fopen (ptr->filename, "r+b")))
	merging = 1;
      else if ((da_file = fopen (ptr->filename, "w+b")))
	;
      else
	{
	  fprintf (stderr, "profiling:%s:Cannot open\n", ptr->filename);
	  ptr->filename = 0;
	  continue;
	}

#if defined (TARGET_HAS_F_SETLKW)
      /* After a fork, another process might try to read and/or write
         the same file simultaneously.  So if we can, lock the file to
         avoid race conditions.  */
      while (fcntl (fileno (da_file), F_SETLKW, &s_flock)
	     && errno == EINTR)
	continue;
#endif
      if (merging)
	{
	  /* Merge data from file.  */
	  unsigned tag, length;
	      
	  if (gcov_read_unsigned (da_file, &tag) || tag != GCOV_DATA_MAGIC)
	    {
	      fprintf (stderr, "profiling:%s:Not a gcov data file\n",
		       ptr->filename);
	    read_fatal:;
	      fclose (da_file);
	      ptr->filename = 0;
	      continue;
	    }
	  if (gcov_read_unsigned (da_file, &length) || length != GCOV_VERSION)
	    {
	      gcov_version_mismatch (ptr, length);
	      goto read_fatal;
	    }
	  
	  /* Merge execution counts for each function.  */
	  count_ptr = ptr->arc_counts;
	  for (ix = ptr->n_functions, fn_info = ptr->functions;
	       ix--; fn_info++)
	    {
	      if (gcov_read_unsigned (da_file, &tag)
		  || gcov_read_unsigned (da_file, &length))
		{
		read_error:;
		  fprintf (stderr, "profiling:%s:Error merging\n",
			   ptr->filename);
		  goto read_fatal;
		}

	      /* Check function */
	      if (tag != GCOV_TAG_FUNCTION)
		{
		read_mismatch:;
		  fprintf (stderr, "profiling:%s:Merge mismatch at %s\n",
			   ptr->filename, fn_info->name);
		  goto read_fatal;
		}
	      {
		unsigned flength, checksum;
		
		if (gcov_read_unsigned (da_file, &flength)
		    || gcov_skip_string (da_file, flength)
		    || gcov_read_unsigned (da_file, &checksum))
		  goto read_error;
		if (flength != strlen (fn_info->name)
		    || checksum != fn_info->checksum)
		  goto read_mismatch;
	      }
	      /* Check arc counts */
	      if (gcov_read_unsigned (da_file, &tag)
		  || gcov_read_unsigned (da_file, &length))
		goto read_error;
	      if (tag != GCOV_TAG_ARC_COUNTS
		  || length / 8 != fn_info->n_arc_counts)
		goto read_mismatch;
	      {
		gcov_type count;
		
		for (jx = fn_info->n_arc_counts; jx--; count_ptr++)
		  if (gcov_read_counter (da_file, &count))
		    goto read_error;
		  else
		    *count_ptr += count;
	      }
	    }

	  /* Check object summary */
	  if (gcov_read_unsigned (da_file, &tag)
	      || gcov_read_unsigned (da_file, &length))
	    goto read_error;
	  if (tag != GCOV_TAG_OBJECT_SUMMARY)
	    goto read_mismatch;
	  if (gcov_read_summary (da_file, &object))
	    goto read_error;

	  /* Check program summary */
	  while (1)
	    {
	      long base = ftell (da_file);
	      
	      if (gcov_read_unsigned (da_file, &tag)
		  || gcov_read_unsigned (da_file, &length))
		{
		  if (feof (da_file))
		    break;
		  goto read_error;
		}
	      if (tag != GCOV_TAG_PROGRAM_SUMMARY
		  && tag != GCOV_TAG_PLACEHOLDER_SUMMARY
		  && tag != GCOV_TAG_INCORRECT_SUMMARY)
		goto read_mismatch;
	      if (gcov_read_summary (da_file, &local_prg))
		goto read_error;
	      if (local_prg.checksum != program.checksum)
		continue;
	      if (tag == GCOV_TAG_PLACEHOLDER_SUMMARY)
		{
		  fprintf (stderr,
			   "profiling:%s:Concurrent race detected\n",
			   ptr->filename);
		  goto read_fatal;
		}
	      merging = -1;
	      if (tag != GCOV_TAG_PROGRAM_SUMMARY)
		break;
	      
	      if (program.runs
		  && memcmp (&program, &local_prg, sizeof (program)))
		{
		  fprintf (stderr, "profiling:%s:Invocation mismatch\n",
			   ptr->filename);
		  local_prg.runs = 0;
		}
	      else
		memcpy (&program, &local_prg, sizeof (program));
	      ptr->wkspc = base;
	      break;
	    }
	  fseek (da_file, 0, SEEK_SET);
	}

      object.runs++;
      object.arcs = ptr->n_arc_counts;
      object.arc_sum = 0;
      if (object.arc_max_one < object_max_one)
	object.arc_max_one = object_max_one;
      object.arc_sum_max += object_max_one;
      
      /* Write out the data.  */
      if (/* magic */
	  gcov_write_unsigned (da_file, GCOV_DATA_MAGIC)
	  /* version number */
	  || gcov_write_unsigned (da_file, GCOV_VERSION))
	{
	write_error:;
	  fclose (da_file);
	  fprintf (stderr, "profiling:%s:Error writing\n", ptr->filename);
	  ptr->filename = 0;
	  continue;
	}
      
      /* Write execution counts for each function.  */
      count_ptr = ptr->arc_counts;
      for (ix = ptr->n_functions, fn_info = ptr->functions; ix--; fn_info++)
	{
	  /* Announce function.  */
	  if (gcov_write_unsigned (da_file, GCOV_TAG_FUNCTION)
	      || !(base = gcov_reserve_length (da_file))
	      /* function name */
	      || gcov_write_string (da_file, fn_info->name,
				    strlen (fn_info->name))
	      /* function checksum */
	      || gcov_write_unsigned (da_file, fn_info->checksum)
	      || gcov_write_length (da_file, base))
	    goto write_error;
	  
	  /* arc counts.  */
	  if (gcov_write_unsigned (da_file, GCOV_TAG_ARC_COUNTS)
	      || !(base = gcov_reserve_length (da_file)))
	    goto write_error;
	  
	  for (jx = fn_info->n_arc_counts; jx--;)
	    {
	      gcov_type count = *count_ptr++;
	      
	      object.arc_sum += count;
	      if (object.arc_max_sum < count)
		object.arc_max_sum = count;
	      if (gcov_write_counter (da_file, count))
		goto write_error; /* RIP Edsger Dijkstra */
	    }
	  if (gcov_write_length (da_file, base))
	    goto write_error;
	}

      /* Object file summary.  */
      if (gcov_write_summary (da_file, GCOV_TAG_OBJECT_SUMMARY, &object))
	goto write_error;

      if (merging >= 0)
	{
	  if (fseek (da_file, 0, SEEK_END))
	    goto write_error;
	  ptr->wkspc = ftell (da_file);
	  if (gcov_write_summary (da_file, GCOV_TAG_PLACEHOLDER_SUMMARY,
				  &program))
	    goto write_error;
	}
      else if (ptr->wkspc)
	{
	  /* Zap trailing program summary */
	  if (fseek (da_file, ptr->wkspc, SEEK_SET))
	    goto write_error;
	  if (!local_prg.runs)
	    ptr->wkspc = 0;
	  if (gcov_write_unsigned (da_file,
			     local_prg.runs ? GCOV_TAG_PLACEHOLDER_SUMMARY
			     : GCOV_TAG_INCORRECT_SUMMARY))
	    goto write_error;
	}
      if (fflush (da_file))
	goto write_error;

      if (fclose (da_file))
	{
	  fprintf (stderr, "profiling:%s:Error closing\n", ptr->filename);
	  ptr->filename = 0;
	}
      else
	{
	  program_arcs += ptr->n_arc_counts;
	  program_sum += object.arc_sum;
	  if (program_max_sum < object.arc_max_sum)
	    program_max_sum = object.arc_max_sum;
	}
    }

  /* Generate whole program statistics.  */
  program.runs++;
  program.arcs = program_arcs;
  program.arc_sum = program_sum;
  if (program.arc_max_one < program_max_one)
    program.arc_max_one = program_max_one;
  if (program.arc_max_sum < program_max_sum)
    program.arc_max_sum = program_max_sum;
  program.arc_sum_max += program_max_one;
  
  /* Upate whole program statistics.  */
  for (ptr = gcov_list; ptr; ptr = ptr->next)
    if (ptr->filename && ptr->wkspc)
      {
	FILE *da_file;
	
	da_file = fopen (ptr->filename, "r+b");
	if (!da_file)
	  {
	    fprintf (stderr, "profiling:%s:Cannot open\n", ptr->filename);
	    continue;
	  }
	
#if defined (TARGET_HAS_F_SETLKW)
	while (fcntl (fileno (da_file), F_SETLKW, &s_flock)
	       && errno == EINTR)
	  continue;
#endif
	if (fseek (da_file, ptr->wkspc, SEEK_SET)
 	    || gcov_write_summary (da_file, GCOV_TAG_PROGRAM_SUMMARY, &program)
 	    || fflush (da_file))
 	  fprintf (stderr, "profiling:%s:Error writing\n", ptr->filename);
	if (fclose (da_file))
	  fprintf (stderr, "profiling:%s:Error closing\n", ptr->filename);
      }
}

/* Add a new object file onto the bb chain.  Invoked automatically
   when running an object file's global ctors.  */

void
__gcov_init (struct gcov_info *info)
{
  if (!info->version)
    return;
  if (info->version != GCOV_VERSION)
    gcov_version_mismatch (info, info->version);
  else
    {
      const char *ptr = info->filename;
      unsigned crc32 = gcov_crc32;
  
      do
	{
	  unsigned ix;
	  unsigned value = *ptr << 24;

	  for (ix = 8; ix--; value <<= 1)
	    {
	      unsigned feedback;

	      feedback = (value ^ crc32) & 0x80000000 ? 0x04c11db7 : 0;
	      crc32 <<= 1;
	      crc32 ^= feedback;
	    }
	}
      while (*ptr++);
      
      gcov_crc32 = crc32;
      
      if (!gcov_list)
	atexit (gcov_exit);
      
      info->next = gcov_list;
      gcov_list = info;
    }
  info->version = 0;
}

/* Called before fork or exec - write out profile information gathered so
   far and reset it to zero.  This avoids duplication or loss of the
   profile information gathered so far.  */

void
__gcov_flush (void)
{
  struct gcov_info *ptr;

  gcov_exit ();
  for (ptr = gcov_list; ptr; ptr = ptr->next)
    {
      unsigned i;
      
      for (i = ptr->n_arc_counts; i--;)
	ptr->arc_counts[i] = 0;
    }
}
