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

#if defined(inhibit_libc)
/* If libc and its header files are not available, provide dummy functions.  */

void __gcov_init (void *p);
void __gcov_flush (void);

void __gcov_init (void *p) { }
void __gcov_flush (void) { }

#else

/* It is incorrect to include config.h here, because this file is being
   compiled for the target, and hence definitions concerning only the host
   do not apply.  */

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"

#undef NULL /* Avoid errors if stdio.h and our stddef.h mismatch.  */
#include <stdio.h>

#include <string.h>
#if defined (TARGET_HAS_F_SETLKW)
#include <fcntl.h>
#include <errno.h>
#endif
#define IN_LIBGCOV 1
#include "gcov-io.h"

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
  
  memset (&program, 0, sizeof (program));
  program.checksum = gcov_crc32;
  
  for (ptr = gcov_list; ptr; ptr = ptr->next)
    {
      struct gcov_summary object;
      struct gcov_summary local_prg;
      int merging;
      long base;
      const struct function_info *fn_info;
      gcov_type **counters;
      gcov_type *count_ptr;
      gcov_type object_max_one = 0;
      gcov_type count;
      unsigned tag, length, flength, checksum;
      unsigned arc_data_index, f_sect_index, sect_index;

      ptr->wkspc = 0;
      if (!ptr->filename)
	continue;

      counters = malloc (sizeof (gcov_type *) * ptr->n_counter_sections);
      for (ix = 0; ix < ptr->n_counter_sections; ix++)
	counters[ix] = ptr->counter_sections[ix].counters;

      for (arc_data_index = 0;
	   arc_data_index < ptr->n_counter_sections
	   && ptr->counter_sections[arc_data_index].tag != GCOV_TAG_ARC_COUNTS;
	   arc_data_index++)
	continue;

      if (arc_data_index == ptr->n_counter_sections)
	{
	  /* For now; later we may want to just measure other profiles,
	     but now I am lazy to check for all consequences.  */
	  abort ();
	}
      for (ix = ptr->counter_sections[arc_data_index].n_counters,
	   count_ptr = ptr->counter_sections[arc_data_index].counters; ix--;)
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
      merging = gcov_open (ptr->filename, 0);
      
      if (!merging)
	{
	  fprintf (stderr, "profiling:%s:Cannot open\n", ptr->filename);
	  ptr->filename = 0;
	  continue;
	}
      
      if (merging > 0)
	{
	  /* Merge data from file.  */
	  if (gcov_read_unsigned (&tag) || tag != GCOV_DATA_MAGIC)
	    {
	      fprintf (stderr, "profiling:%s:Not a gcov data file\n",
		       ptr->filename);
	    read_fatal:;
	      gcov_close ();
	      ptr->filename = 0;
	      continue;
	    }
	  if (gcov_read_unsigned (&length) || length != GCOV_VERSION)
	    {
	      gcov_version_mismatch (ptr, length);
	      goto read_fatal;
	    }
	  
	  /* Merge execution counts for each function.  */
	  for (ix = ptr->n_functions, fn_info = ptr->functions;
	       ix--; fn_info++)
	    {
	      if (gcov_read_unsigned (&tag) || gcov_read_unsigned (&length))
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

	      if (gcov_read_unsigned (&flength)
		  || gcov_skip_string (flength)
		  || gcov_read_unsigned (&checksum))
		goto read_error;
	      if (flength != strlen (fn_info->name)
		  || checksum != fn_info->checksum)
		goto read_mismatch;

	      /* Counters.  */
	      for (f_sect_index = 0;
		   f_sect_index < fn_info->n_counter_sections;
		   f_sect_index++)
		{
		  unsigned n_counters;

		  if (gcov_read_unsigned (&tag)
		      || gcov_read_unsigned (&length))
		    goto read_error;
		  for (sect_index = 0;
		       sect_index < ptr->n_counter_sections;
		       sect_index++)
		    if (ptr->counter_sections[sect_index].tag == tag)
		      break;
		  if (sect_index == ptr->n_counter_sections
		      || fn_info->counter_sections[f_sect_index].tag != tag)
		    goto read_mismatch;

		  n_counters = fn_info->counter_sections[f_sect_index].n_counters;
		  if (n_counters != length / 8)
		    goto read_mismatch;
		 
		  for (jx = 0; jx < n_counters; jx++)
	    	    if (gcov_read_counter (&count))
    		      goto read_error;
		    else
		      counters[sect_index][jx] += count;
		  counters[sect_index] += n_counters;
		}
	    }

	  /* Check object summary */
	  if (gcov_read_unsigned (&tag) || gcov_read_unsigned (&length))
	    goto read_error;
	  if (tag != GCOV_TAG_OBJECT_SUMMARY)
	    goto read_mismatch;
	  if (gcov_read_summary (&object))
	    goto read_error;

	  /* Check program summary */
	  while (1)
	    {
	      long base = gcov_save_position ();
	      
	      if (gcov_read_unsigned (&tag)
		  || gcov_read_unsigned (&length))
		{
		  if (gcov_eof ())
		    break;
		  goto read_error;
		}
	      if (tag != GCOV_TAG_PROGRAM_SUMMARY
		  && tag != GCOV_TAG_PLACEHOLDER_SUMMARY
		  && tag != GCOV_TAG_INCORRECT_SUMMARY)
		goto read_mismatch;
	      if (gcov_read_summary (&local_prg))
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
	      merging = 0;
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
	  gcov_resync (0, 0);
	}

      object.runs++;
      object.arcs = ptr->counter_sections[arc_data_index].n_counters;
      object.arc_sum = 0;
      if (object.arc_max_one < object_max_one)
	object.arc_max_one = object_max_one;
      object.arc_sum_max += object_max_one;
      
      /* Write out the data.  */
      if (/* magic */
	  gcov_write_unsigned (GCOV_DATA_MAGIC)
	  /* version number */
	  || gcov_write_unsigned (GCOV_VERSION))
	{
	write_error:;
	  gcov_close ();
	  fprintf (stderr, "profiling:%s:Error writing\n", ptr->filename);
	  ptr->filename = 0;
	  continue;
	}
      
      /* Write execution counts for each function.  */
      for (ix = 0; ix < ptr->n_counter_sections; ix++)
	counters[ix] = ptr->counter_sections[ix].counters;
      for (ix = ptr->n_functions, fn_info = ptr->functions; ix--; fn_info++)
	{
	  /* Announce function.  */
	  if (gcov_write_unsigned (GCOV_TAG_FUNCTION)
	      || !(base = gcov_reserve_length ())
	      /* function name */
	      || gcov_write_string (fn_info->name)
	      /* function checksum */
	      || gcov_write_unsigned (fn_info->checksum)
	      || gcov_write_length (base))
	    goto write_error;

	  /* counters.  */
	  for (f_sect_index = 0;
	       f_sect_index < fn_info->n_counter_sections;
	       f_sect_index++)
	    {
	      tag = fn_info->counter_sections[f_sect_index].tag;
	      for (sect_index = 0;
    		   sect_index < ptr->n_counter_sections;
		   sect_index++)
		if (ptr->counter_sections[sect_index].tag == tag)
		  break;
	      if (sect_index == ptr->n_counter_sections)
		abort ();

	      if (gcov_write_unsigned (tag)
		  || !(base = gcov_reserve_length ()))
		goto write_error;
	  
    	      for (jx = fn_info->counter_sections[f_sect_index].n_counters; jx--;)
		{
		  gcov_type count = *counters[sect_index]++;
	      
		  if (tag == GCOV_TAG_ARC_COUNTS)
		    {
		      object.arc_sum += count;
		      if (object.arc_max_sum < count)
			object.arc_max_sum = count;
		    }
		  if (gcov_write_counter (count))
		    goto write_error; /* RIP Edsger Dijkstra */
		}
	      if (gcov_write_length (base))
		goto write_error;
	    }
	}

      /* Object file summary.  */
      if (gcov_write_summary (GCOV_TAG_OBJECT_SUMMARY, &object))
	goto write_error;

      if (merging)
	{
	  ptr->wkspc = gcov_seek_end ();
	  if (gcov_write_summary (GCOV_TAG_PLACEHOLDER_SUMMARY,
				  &program))
	    goto write_error;
	}
      else if (ptr->wkspc)
	{
	  /* Zap trailing program summary */
	  if (gcov_resync (ptr->wkspc, 0))
	    goto write_error;
	  if (!local_prg.runs)
	    ptr->wkspc = 0;
	  if (gcov_write_unsigned (local_prg.runs
				   ? GCOV_TAG_PLACEHOLDER_SUMMARY
				   : GCOV_TAG_INCORRECT_SUMMARY))
	    goto write_error;
	}

      if (gcov_close ())
	{
	  fprintf (stderr, "profiling:%s:Error closing\n", ptr->filename);
	  ptr->filename = 0;
	}
      else
	{
	  program_arcs += ptr->counter_sections[arc_data_index].n_counters;
	  program_sum += object.arc_sum;
	  if (program_max_sum < object.arc_max_sum)
	    program_max_sum = object.arc_max_sum;
	}
      free(counters);
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
	if (!gcov_open (ptr->filename, 1))
	  {
	    fprintf (stderr, "profiling:%s:Cannot open\n", ptr->filename);
	    continue;
	  }
	
	if (gcov_resync (ptr->wkspc, 0)
 	    || gcov_write_summary (GCOV_TAG_PROGRAM_SUMMARY, &program))
 	  fprintf (stderr, "profiling:%s:Error writing\n", ptr->filename);
	if (gcov_close ())
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
      unsigned i, j;
      
      for (j = 0; j < ptr->n_counter_sections; j++)
	for (i = ptr->counter_sections[j].n_counters; i--;)
	  ptr->counter_sections[j].counters[i] = 0;
    }
}

#endif /* inhibit_libc */
