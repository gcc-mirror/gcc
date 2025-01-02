/* Platform-independent deterministic sort function.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by Alexander Monakov.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This implements a sort function suitable for GCC use cases:
   - signature-compatible to C qsort, but relaxed contract:
     - may apply the comparator to elements in a temporary buffer
     - may abort on allocation failure
   - deterministic (but not necessarily stable)
   - fast, especially for common cases (0-5 elements of size 8 or 4)

   The implementation uses sorting networks for up to 5 elements and
   a merge sort on top of that.  Neither stage has branches depending on
   comparator result, trading extra arithmetic for branch mispredictions.  */

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

#include "system.h"

#ifdef __GNUC__
#define noinline __attribute__ ((__noinline__))
#else
#define noinline
#endif

/* C-style qsort comparator function type.  */
typedef int cmp_fn (const void *, const void *);

/* Structure holding read-mostly (read-only in netsort) context.  */
struct sort_ctx
{
  cmp_fn *cmp; // pointer to comparator
  char   *out; // output buffer
  size_t n;    // number of elements
  size_t size; // element size
  size_t nlim; // limit for using sorting networks
};

/* Like sort_ctx, but for use with qsort_r-style comparators.  Several
   functions in this file are templates that work with either context type.  */
struct sort_r_ctx
{
  void          *data;
  sort_r_cmp_fn *cmp_;
  char   *out;
  size_t n;
  size_t size;
  size_t nlim;
  int cmp (const void *a, const void *b)
  {
    return cmp_ (a, b, data);
  }
};

/* Helper for netsort. Permute, possibly in-place, 2 or 3 elements,
   placing E0 to C->OUT, E1 to C->OUT + C->SIZE, and so on.  */
template<typename sort_ctx>
static void
reorder23 (sort_ctx *c, char *e0, char *e1, char *e2)
{
#define REORDER_23(TYPE, STRIDE, OFFSET)                 \
do {                                                     \
  TYPE t0, t1;                                           \
  memcpy (&t0, e0 + OFFSET, sizeof (TYPE));              \
  memcpy (&t1, e1 + OFFSET, sizeof (TYPE));              \
  char *out = c->out + OFFSET;                           \
  if (LIKELY (c->n == 3))                                \
    memmove (out + 2*STRIDE, e2 + OFFSET, sizeof (TYPE));\
  memcpy (out, &t0, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t1, sizeof (TYPE));                      \
} while (0)

  if (LIKELY (c->size == sizeof (size_t)))
    REORDER_23 (size_t, sizeof (size_t), 0);
  else if (LIKELY (c->size == sizeof (int)))
    REORDER_23 (int, sizeof (int), 0);
  else
    {
      size_t offset = 0, step = sizeof (size_t);
      for (; offset + step <= c->size; offset += step)
	REORDER_23 (size_t, c->size, offset);
      for (; offset < c->size; offset++)
	REORDER_23 (char, c->size, offset);
    }
}

/* Like reorder23, but permute 4 or 5 elements.  */
template<typename sort_ctx>
static void
reorder45 (sort_ctx *c, char *e0, char *e1, char *e2, char *e3, char *e4)
{
#define REORDER_45(TYPE, STRIDE, OFFSET)                 \
do {                                                     \
  TYPE t0, t1, t2, t3;                                   \
  memcpy (&t0, e0 + OFFSET, sizeof (TYPE));              \
  memcpy (&t1, e1 + OFFSET, sizeof (TYPE));              \
  memcpy (&t2, e2 + OFFSET, sizeof (TYPE));              \
  memcpy (&t3, e3 + OFFSET, sizeof (TYPE));              \
  char *out = c->out + OFFSET;                           \
  if (LIKELY (c->n == 5))                                \
    memmove (out + 4*STRIDE, e4 + OFFSET, sizeof (TYPE));\
  memcpy (out, &t0, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t1, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t2, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t3, sizeof (TYPE));                      \
} while (0)

  if (LIKELY (c->size == sizeof (size_t)))
    REORDER_45 (size_t, sizeof (size_t), 0);
  else if (LIKELY (c->size == sizeof (int)))
    REORDER_45 (int,  sizeof (int), 0);
  else
    {
      size_t offset = 0, step = sizeof (size_t);
      for (; offset + step <= c->size; offset += step)
	REORDER_45 (size_t, c->size, offset);
      for (; offset < c->size; offset++)
	REORDER_45 (char, c->size, offset);
    }
}

/* Helper for netsort. Invoke comparator CMP on E0 and E1.
   Return E0^E1 if E0 compares less than E1, zero otherwise.
   This is noinline to avoid code growth and confine invocation
   to a single call site, assisting indirect branch prediction.  */
template<typename sort_ctx>
noinline static intptr_t
cmp1 (char *e0, char *e1, sort_ctx *c)
{
  intptr_t x = (intptr_t)e0 ^ (intptr_t)e1;
  return x & (c->cmp (e0, e1) >> 31);
}

/* Apply a sorting network to 2 to 5 elements from IN, placing them into C->OUT.
   IN may be equal to C->OUT, in which case elements are sorted in place.  */
template<typename sort_ctx>
static void
netsort (char *in, sort_ctx *c)
{
#define CMP(e0, e1)                   \
do {                                  \
  intptr_t x = cmp1 (e1, e0, c);      \
  e0 = (char *)((intptr_t)e0 ^ x);    \
  e1 = (char *)((intptr_t)e1 ^ x);    \
} while (0)

  char *e0 = in, *e1 = e0 + c->size, *e2 = e1 + c->size;
  CMP (e0, e1);
  if (LIKELY (c->n == 3))
    {
      CMP (e1, e2);
      CMP (e0, e1);
    }
  if (c->n <= 3)
    return reorder23 (c, e0, e1, e2);
  char *e3 = e2 + c->size, *e4 = e3 + c->size;
  if (LIKELY (c->n == 5))
    {
      CMP (e3, e4);
      CMP (e2, e4);
    }
  CMP (e2, e3);
  if (LIKELY (c->n == 5))
    {
      CMP (e0, e3);
      CMP (e1, e4);
    }
  CMP (e0, e2);
  CMP (e1, e3);
  CMP (e1, e2);
  reorder45 (c, e0, e1, e2, e3, e4);
}

/* Execute merge sort on N elements from IN, placing them into OUT,
   using TMP as temporary storage if IN is equal to OUT.
   This is a stable sort if netsort is used only for 2 or 3 elements.  */
template<typename sort_ctx>
static void
mergesort (char *in, sort_ctx *c, size_t n, char *out, char *tmp)
{
  if (LIKELY (n <= c->nlim))
    {
      c->out = out;
      c->n = n;
      return netsort (in, c);
    }
  size_t nl = n / 2, nr = n - nl, sz = nl * c->size;
  char *mid = in + sz, *r = out + sz, *l = in == out ? tmp : in;
  /* Sort the right half, outputting to right half of OUT.  */
  mergesort (mid, c, nr, r, tmp);
  /* Sort the left half, leaving left half of OUT free.  */
  mergesort (in, c, nl, l, mid);
  /* Merge sorted halves given by L, R to [OUT, END).  */
#define MERGE_ELTSIZE(SIZE)                     \
do {                                            \
  intptr_t mr = c->cmp (r, l) >> 31;            \
  intptr_t lr = (intptr_t)l ^ (intptr_t)r;      \
  lr = (intptr_t)l ^ (lr & mr);                 \
  out = (char *)memcpy (out, (char *)lr, SIZE); \
  out += SIZE;                                  \
  r += mr & SIZE;                               \
  if (r == out) return;                         \
  l += ~mr & SIZE;                              \
} while (r != end)

  if (LIKELY (c->cmp (r, l + (r - out) - c->size) < 0))
    {
      char *end = out + n * c->size;
      if (sizeof (size_t) == 8 && LIKELY (c->size == 8))
	MERGE_ELTSIZE (8);
      else if (LIKELY (c->size == 4))
	MERGE_ELTSIZE (4);
      else
	MERGE_ELTSIZE (c->size);
    }
  memcpy (out, l, r - out);
}

#if CHECKING_P
  /* Don't complain about cast from void* to function pointer.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconditionally-supported"

/* Adapter for using two-argument comparators in functions expecting the
   three-argument sort_r_cmp_fn type.  */
static int
cmp2to3 (const void *a, const void *b, void *c)
{
  return ((cmp_fn *)c) (a, b);
}
#endif

/* Replacement for C qsort.  */
void
gcc_qsort (void *vbase, size_t n, size_t size, cmp_fn *cmp)
{
  if (n < 2)
    return;
  size_t nlim = 5;
  bool stable = (ssize_t) size < 0;
  if (stable)
    nlim = 3, size = ~size;
  char *base = (char *)vbase;
  sort_ctx c = {cmp, base, n, size, nlim};
  long long scratch[32];
  size_t bufsz = (n / 2) * size;
  void *buf = bufsz <= sizeof scratch ? scratch : xmalloc (bufsz);
  mergesort (base, &c, n, base, (char *)buf);
  if (buf != scratch)
    free (buf);
#if CHECKING_P
  qsort_chk (vbase, n, size, cmp2to3, (void*)cmp);
#pragma GCC diagnostic pop
#endif
}

/* Substitute for Glibc qsort_r.  */
void
gcc_sort_r (void *vbase, size_t n, size_t size, sort_r_cmp_fn *cmp, void *data)
{
  if (n < 2)
    return;
  size_t nlim = 5;
  bool stable = (ssize_t) size < 0;
  if (stable)
    nlim = 3, size = ~size;
  char *base = (char *)vbase;
  sort_r_ctx c = {data, cmp, base, n, size, nlim};
  long long scratch[32];
  size_t bufsz = (n / 2) * size;
  void *buf = bufsz <= sizeof scratch ? scratch : xmalloc (bufsz);
  mergesort (base, &c, n, base, (char *)buf);
  if (buf != scratch)
    free (buf);
#if CHECKING_P
  qsort_chk (vbase, n, size, cmp, data);
#endif
}

/* Stable sort, signature-compatible to C qsort.  */
void
gcc_stablesort (void *vbase, size_t n, size_t size, cmp_fn *cmp)
{
  gcc_qsort (vbase, n, ~size, cmp);
}

/* Stable sort, signature-compatible to Glibc qsort_r.  */
void
gcc_stablesort_r (void *vbase, size_t n, size_t size, sort_r_cmp_fn *cmp,
		  void *data)
{
  gcc_sort_r (vbase, n, ~size, cmp, data);
}
