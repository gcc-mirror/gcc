/* Subroutines needed for unwinding stack frames for exception handling.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@cygnus.com>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Sorting an array of FDEs by address.
   (Ideally we would have the linker sort the FDEs so we don't have to do
   it at run time. But the linkers are not yet prepared for this.)  */

/* This is a special mix of insertion sort and heap sort, optimized for
   the data sets that actually occur. They look like
   101 102 103 127 128 105 108 110 190 111 115 119 125 160 126 129 130.
   I.e. a linearly increasing sequence (coming from functions in the text
   section), with additionally a few unordered elements (coming from functions
   in gnu_linkonce sections) whose values are higher than the values in the
   surrounding linear sequence (but not necessarily higher than the values
   at the end of the linear sequence!).
   The worst-case total run time is O(N) + O(n log (n)), where N is the
   total number of FDEs and n is the number of erratic ones.  */

typedef struct fde_vector
{
  fde **array;
  size_t count;
} fde_vector;

typedef struct fde_accumulator
{
  fde_vector linear;
  fde_vector erratic;
} fde_accumulator;

static inline int
start_fde_sort (fde_accumulator *accu, size_t count)
{
  accu->linear.array = (fde **) malloc (sizeof (fde *) * count);
  accu->erratic.array = accu->linear.array ?
      (fde **) malloc (sizeof (fde *) * count) : NULL;
  accu->linear.count = 0;
  accu->erratic.count = 0;
  
  return accu->linear.array != NULL;
}

static inline void
fde_insert (fde_accumulator *accu, fde *this_fde)
{
  if (accu->linear.array)
    accu->linear.array[accu->linear.count++] = this_fde;
}

/* Split LINEAR into a linear sequence with low values and an erratic
   sequence with high values, put the linear one (of longest possible
   length) into LINEAR and the erratic one into ERRATIC. This is O(N).
   
   Because the longest linear sequence we are trying to locate within the
   incoming LINEAR array can be interspersed with (high valued) erratic
   entries.  We construct a chain indicating the sequenced entries.
   To avoid having to allocate this chain, we overlay it onto the space of
   the ERRATIC array during construction.  A final pass iterates over the
   chain to determine what should be placed in the ERRATIC array, and
   what is the linear sequence.  This overlay is safe from aliasing.  */
static inline void
fde_split (fde_vector *linear, fde_vector *erratic)
{
  static fde *marker;
  size_t count = linear->count;
  fde **chain_end = &marker;
  size_t i, j, k;

  /* This should optimize out, but it is wise to make sure this assumption
     is correct. Should these have different sizes, we cannot cast between
     them and the overlaying onto ERRATIC will not work.  */
  if (sizeof (fde *) != sizeof (fde **))
    abort ();
  
  for (i = 0; i < count; i++)
    {
      fde **probe;
      
      for (probe = chain_end;
           probe != &marker && fde_compare (linear->array[i], *probe) < 0;
           probe = chain_end)
        {
          chain_end = (fde **)erratic->array[probe - linear->array];
          erratic->array[probe - linear->array] = NULL;
        }
      erratic->array[i] = (fde *)chain_end;
      chain_end = &linear->array[i];
    }

  /* Each entry in LINEAR which is part of the linear sequence we have
     discovered will correspond to a non-NULL entry in the chain we built in
     the ERRATIC array.  */
  for (i = j = k = 0; i < count; i++)
    if (erratic->array[i])
      linear->array[j++] = linear->array[i];
    else
      erratic->array[k++] = linear->array[i];
  linear->count = j;
  erratic->count = k;
}

/* This is O(n log(n)).  BSD/OS defines heapsort in stdlib.h, so we must
   use a name that does not conflict.  */
static inline void
frame_heapsort (fde_vector *erratic)
{
  /* For a description of this algorithm, see:
     Samuel P. Harbison, Guy L. Steele Jr.: C, a reference manual, 2nd ed.,
     p. 60-61. */
  fde ** a = erratic->array;
  /* A portion of the array is called a "heap" if for all i>=0:
     If i and 2i+1 are valid indices, then a[i] >= a[2i+1].
     If i and 2i+2 are valid indices, then a[i] >= a[2i+2]. */
#define SWAP(x,y) do { fde * tmp = x; x = y; y = tmp; } while (0)
  size_t n = erratic->count;
  size_t m = n;
  size_t i;

  while (m > 0)
    {
      /* Invariant: a[m..n-1] is a heap. */
      m--;
      for (i = m; 2*i+1 < n; )
        {
          if (2*i+2 < n
              && fde_compare (a[2*i+2], a[2*i+1]) > 0
              && fde_compare (a[2*i+2], a[i]) > 0)
            {
              SWAP (a[i], a[2*i+2]);
              i = 2*i+2;
            }
          else if (fde_compare (a[2*i+1], a[i]) > 0)
            {
              SWAP (a[i], a[2*i+1]);
              i = 2*i+1;
            }
          else
            break;
        }
    }
  while (n > 1)
    {
      /* Invariant: a[0..n-1] is a heap. */
      n--;
      SWAP (a[0], a[n]);
      for (i = 0; 2*i+1 < n; )
        {
          if (2*i+2 < n
              && fde_compare (a[2*i+2], a[2*i+1]) > 0
              && fde_compare (a[2*i+2], a[i]) > 0)
            {
              SWAP (a[i], a[2*i+2]);
              i = 2*i+2;
            }
          else if (fde_compare (a[2*i+1], a[i]) > 0)
            {
              SWAP (a[i], a[2*i+1]);
              i = 2*i+1;
            }
          else
            break;
        }
    }
#undef SWAP
}

/* Merge V1 and V2, both sorted, and put the result into V1. */
static void
fde_merge (fde_vector *v1, const fde_vector *v2)
{
  size_t i1, i2;
  fde * fde2;

  i2 = v2->count;
  if (i2 > 0)
    {
      i1 = v1->count;
      do {
        i2--;
        fde2 = v2->array[i2];
        while (i1 > 0 && fde_compare (v1->array[i1-1], fde2) > 0)
          {
            v1->array[i1+i2] = v1->array[i1-1];
            i1--;
          }
        v1->array[i1+i2] = fde2;
      } while (i2 > 0);
      v1->count += v2->count;
    }
}

static fde **
end_fde_sort (fde_accumulator *accu, size_t count)
{
  if (accu->linear.array && accu->linear.count != count)
    abort ();
  
  if (accu->erratic.array)
    {
      fde_split (&accu->linear, &accu->erratic);
      if (accu->linear.count + accu->erratic.count != count)
	abort ();
      frame_heapsort (&accu->erratic);
      fde_merge (&accu->linear, &accu->erratic);
      if (accu->erratic.array)
        free (accu->erratic.array);
    }
  else
    {
      /* We've not managed to malloc an erratic array, so heap sort in the
         linear one.  */
      frame_heapsort (&accu->linear);
    }
  return accu->linear.array;
}

/* Called from crtbegin.o to register the unwind info for an object.  */

void
__register_frame_info (void *begin, struct object *ob)
{
  ob->fde_begin = begin;

  ob->pc_begin = ob->pc_end = 0;
  ob->fde_array = 0;
  ob->count = 0;

  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  ob->next = objects;
  objects = ob;

  __gthread_mutex_unlock (&object_mutex);
}

void
__register_frame (void *begin)
{
  struct object *ob = (struct object *) malloc (sizeof (struct object));
  __register_frame_info (begin, ob);                       
}

/* Similar, but BEGIN is actually a pointer to a table of unwind entries
   for different translation units.  Called from the file generated by
   collect2.  */

void
__register_frame_info_table (void *begin, struct object *ob)
{
  ob->fde_begin = begin;
  ob->fde_array = begin;

  ob->pc_begin = ob->pc_end = 0;
  ob->count = 0;

  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  ob->next = objects;
  objects = ob;

  __gthread_mutex_unlock (&object_mutex);
}

void
__register_frame_table (void *begin)
{
  struct object *ob = (struct object *) malloc (sizeof (struct object));
  __register_frame_info_table (begin, ob);
}

/* Called from crtbegin.o to deregister the unwind info for an object.  */

void *
__deregister_frame_info (void *begin)
{
  struct object **p;

  init_object_mutex_once ();
  __gthread_mutex_lock (&object_mutex);

  p = &objects;
  while (*p)
    {
      if ((*p)->fde_begin == begin)
	{
	  struct object *ob = *p;
	  *p = (*p)->next;

	  /* If we've run init_frame for this object, free the FDE array.  */
	  if (ob->fde_array && ob->fde_array != begin)
	    free (ob->fde_array);

	  __gthread_mutex_unlock (&object_mutex);
	  return (void *) ob;
	}
      p = &((*p)->next);
    }

  __gthread_mutex_unlock (&object_mutex);
  abort ();
}

void
__deregister_frame (void *begin)
{
  free (__deregister_frame_info (begin));
}

