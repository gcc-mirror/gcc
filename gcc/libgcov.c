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
#include "gcov-io.c"

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
  struct gcov_info *gi_ptr;
  struct gcov_summary this_program;
  struct gcov_summary all;

  memset (&all, 0, sizeof (all));
  /* Find the totals for this execution.  */
  memset (&this_program, 0, sizeof (this_program));
  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      const struct gcov_ctr_info *ci_ptr;
      struct gcov_ctr_summary *cs_ptr;
      unsigned t_ix;
      
      for (t_ix = 0, ci_ptr = gi_ptr->counts, cs_ptr = this_program.ctrs;
	   t_ix != GCOV_COUNTERS; t_ix++, cs_ptr++)
	if ((1 << t_ix) & gi_ptr->ctr_mask)
	  {
	    const gcov_type *c_ptr;
	    unsigned c_num;

	    cs_ptr->num += ci_ptr->num;
	    for (c_num = ci_ptr->num, c_ptr = ci_ptr->values; c_num--; c_ptr++)
	      {
		cs_ptr->sum_all += *c_ptr;
		if (cs_ptr->run_max < *c_ptr)
		  cs_ptr->run_max = *c_ptr;
	      }
	    ci_ptr++;
	  }
    }

  /* Now write the data  */
  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      struct gcov_summary this_object;
      struct gcov_summary object, program;
      gcov_type *values[GCOV_COUNTERS];
      const struct gcov_fn_info *fi_ptr;
      unsigned fi_stride;
      unsigned c_ix, t_ix, f_ix;
      const struct gcov_ctr_info *ci_ptr;
      struct gcov_ctr_summary *cs_ptr;
      struct gcov_ctr_summary *cs_obj, *cs_tobj, *cs_prg, *cs_tprg, *cs_all;
      int error;
      int merging;
      unsigned long base;
      unsigned tag, length;
      unsigned long summary_pos = 0;

      /* Totals for this object file.  */
      memset (&this_object, 0, sizeof (this_object));
      for (t_ix = c_ix = 0,
	     ci_ptr = gi_ptr->counts, cs_ptr = this_object.ctrs;
	   t_ix != GCOV_COUNTERS; t_ix++, cs_ptr++)
	if ((1 << t_ix) & gi_ptr->ctr_mask)
	  {
	    const gcov_type *c_ptr;
	    unsigned c_num;

	    cs_ptr->num += ci_ptr->num;
	    values[c_ix] = ci_ptr->values;
	    for (c_num = ci_ptr->num, c_ptr = ci_ptr->values; c_num--; c_ptr++)
	      {
		cs_ptr->sum_all += *c_ptr;
		if (cs_ptr->run_max < *c_ptr)
		  cs_ptr->run_max = *c_ptr;
	      }
	    c_ix++;
	    ci_ptr++;
	  }

      /* Calculate the function_info stride. This depends on the
	 number of counter types being measured.  */
      fi_stride = sizeof (struct gcov_fn_info) + c_ix * sizeof (unsigned);
      if (__alignof__ (struct gcov_fn_info) > sizeof (unsigned))
	{
	  fi_stride += __alignof__ (struct gcov_fn_info) - 1;
	  fi_stride &= ~(__alignof__ (struct gcov_fn_info) - 1);
	}
      
      /* Open for modification, if possible */
      merging = gcov_open (gi_ptr->filename, 0);
      if (!merging)
	{
	  fprintf (stderr, "profiling:%s:Cannot open\n", gi_ptr->filename);
	  continue;
	}
      
      if (merging > 0)
	{
	  /* Merge data from file.  */
	  if (gcov_read_unsigned () != GCOV_DATA_MAGIC)
	    {
	      fprintf (stderr, "profiling:%s:Not a gcov data file\n",
		       gi_ptr->filename);
	    read_fatal:;
	      gcov_close ();
	      continue;
	    }
	  length = gcov_read_unsigned ();
	  if (length != GCOV_VERSION)
	    {
	      gcov_version_mismatch (gi_ptr, length);
	      goto read_fatal;
	    }
	  
	  /* Merge execution counts for each function.  */
	  for (f_ix = gi_ptr->n_functions, fi_ptr = gi_ptr->functions; f_ix--;
	       fi_ptr = (const struct gcov_fn_info *)
		 ((const char *) fi_ptr + fi_stride))
	    {
	      tag = gcov_read_unsigned ();
	      length = gcov_read_unsigned ();

	      /* Check function */
	      if (tag != GCOV_TAG_FUNCTION)
		{
		read_mismatch:;
		  fprintf (stderr, "profiling:%s:Merge mismatch for %s\n",
			   gi_ptr->filename,
			   fi_ptr ? fi_ptr->name : "summaries");
		  goto read_fatal;
		}

	      if (strcmp (gcov_read_string (), fi_ptr->name)
		  || gcov_read_unsigned () != fi_ptr->checksum)
		goto read_mismatch;

	      for (c_ix = t_ix = 0; t_ix != GCOV_COUNTERS; t_ix++)
		if ((1 << t_ix) & gi_ptr->ctr_mask)
		  {
		    unsigned n_counts;
		    gcov_type *c_ptr;
		    
		    tag = gcov_read_unsigned ();
		    length = gcov_read_unsigned ();

		    if (tag != GCOV_TAG_FOR_COUNTER (t_ix)
			|| fi_ptr->n_ctrs[c_ix] * 8 != length)
		      goto read_mismatch;
		    c_ptr = values[c_ix];
		    for (n_counts = fi_ptr->n_ctrs[c_ix];
			 n_counts--; c_ptr++)
		      *c_ptr += gcov_read_counter ();
		    values[c_ix] = c_ptr;
		    c_ix++;
		}
	      if ((error = gcov_is_error ()))
		goto read_error;
	    }

	  /* Check object summary */
	  if (gcov_read_unsigned () != GCOV_TAG_OBJECT_SUMMARY)
	    goto read_mismatch;
	  gcov_read_unsigned ();
	  gcov_read_summary (&object);

	  /* Check program summary */
	  while (!gcov_is_eof ())
	    {
	      base = gcov_position ();
	      tag = gcov_read_unsigned ();
	      gcov_read_unsigned ();
	      if (tag != GCOV_TAG_PROGRAM_SUMMARY)
		goto read_mismatch;
	      gcov_read_summary (&program);
	      if ((error = gcov_is_error ()))
		{
		read_error:;
		  fprintf (stderr, error < 0 ?
			   "profiling:%s:Overflow merging\n" :
			   "profiling:%s:Error merging\n",
			   gi_ptr->filename);
		  goto read_fatal;
		}
	      
	      if (program.checksum != gcov_crc32)
		continue;
	      summary_pos = base;
	      break;
	    }
	  gcov_seek (0, 0);
	}
      else
	memset (&object, 0, sizeof (object));
      if (!summary_pos)
	memset (&program, 0, sizeof (program));

      fi_ptr = 0;
      
      /* Merge the summaries.  */
      for (t_ix = c_ix = 0,
	     cs_obj = object.ctrs, cs_tobj = this_object.ctrs,
	     cs_prg = program.ctrs, cs_tprg = this_program.ctrs,
	     cs_all = all.ctrs;
	   t_ix != GCOV_COUNTERS;
	   t_ix++, cs_obj++, cs_tobj++, cs_prg++, cs_tprg++, cs_all++)
	{
	  if ((1 << t_ix) & gi_ptr->ctr_mask)
	    {
	      if (!cs_obj->runs++)
		cs_obj->num = cs_tobj->num;
	      else if (cs_obj->num != cs_tobj->num)
		goto read_mismatch;
	      cs_obj->sum_all += cs_tobj->sum_all;
	      if (cs_obj->run_max < cs_tobj->run_max)
		cs_obj->run_max = cs_tobj->run_max;
	      cs_obj->sum_max += cs_tobj->run_max;
	      
	      if (!cs_prg->runs++)
		cs_prg->num = cs_tprg->num;
	      else if (cs_prg->num != cs_tprg->num)
		goto read_mismatch;
	      cs_prg->sum_all += cs_tprg->sum_all;
	      if (cs_prg->run_max < cs_tprg->run_max)
		cs_prg->run_max = cs_tprg->run_max;
	      cs_prg->sum_max += cs_tprg->run_max;
	      
	      values[c_ix] = gi_ptr->counts[c_ix].values;
	      c_ix++;
	    }
	  else if (cs_obj->num || cs_prg->num)
	    goto read_mismatch;
	  
	  if (!cs_all->runs && cs_prg->runs)
	    memcpy (cs_all, cs_prg, sizeof (*cs_all));
	  else if (!all.checksum
		   && (!GCOV_LOCKED || cs_all->runs == cs_prg->runs)
		   && memcmp (cs_all, cs_prg, sizeof (*cs_all)))
	    {
	      fprintf (stderr, "profiling:%s:Invocation mismatch - some data files may have been removed%s",
		       gi_ptr->filename, GCOV_LOCKED
		       ? "" : " or concurrent update without locking support");
	      all.checksum = ~0u;
	    }
	}
      
      program.checksum = gcov_crc32;
      
      /* Write out the data.  */
      gcov_write_unsigned (GCOV_DATA_MAGIC);
      gcov_write_unsigned (GCOV_VERSION);
      
      /* Write execution counts for each function.  */
      for (f_ix = gi_ptr->n_functions, fi_ptr = gi_ptr->functions; f_ix--;
	   fi_ptr = (const struct gcov_fn_info *)
	     ((const char *) fi_ptr + fi_stride))
	{
	  /* Announce function.  */
	  base = gcov_write_tag (GCOV_TAG_FUNCTION);
	  gcov_write_string (fi_ptr->name);
	  gcov_write_unsigned (fi_ptr->checksum);
	  gcov_write_length (base);

	  for (c_ix = t_ix = 0; t_ix != GCOV_COUNTERS; t_ix++)
	    if ((1 << t_ix) & gi_ptr->ctr_mask)
	      {
		unsigned n_counts;
		gcov_type *c_ptr;
		    
		base = gcov_write_tag (GCOV_TAG_FOR_COUNTER (t_ix));
		c_ptr = values[c_ix];
		for (n_counts = fi_ptr->n_ctrs[c_ix]; n_counts--; c_ptr++)
		  gcov_write_counter (*c_ptr);
		values[c_ix] = c_ptr;
		gcov_write_length (base);
		c_ix++;
	      }
	}

      /* Object file summary.  */
      gcov_write_summary (GCOV_TAG_OBJECT_SUMMARY, &object);

      /* Generate whole program statistics.  */
      if (summary_pos)
	gcov_seek (summary_pos, 0);
      else
	gcov_seek_end ();
      gcov_write_summary (GCOV_TAG_PROGRAM_SUMMARY, &program);
      if ((error = gcov_close ()))
	{
	  fprintf (stderr, error  < 0 ?
		   "profiling:%s:Overflow writing\n" :
		   "profiling:%s:Error writing\n",
		   gi_ptr->filename);
	  gi_ptr->filename = 0;
	}
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
  const struct gcov_info *gi_ptr;

  gcov_exit ();
  for (gi_ptr = gcov_list; gi_ptr; gi_ptr = gi_ptr->next)
    {
      unsigned t_ix;
      const struct gcov_ctr_info *ci_ptr;
      
      for (t_ix = 0, ci_ptr = gi_ptr->counts; t_ix != GCOV_COUNTERS; t_ix++)
	if ((1 << t_ix) & gi_ptr->ctr_mask)
	  {
	    memset (ci_ptr->values, 0, sizeof (gcov_type) * ci_ptr->num);
	    ci_ptr++;
	  }
    }
}

#endif /* inhibit_libc */
