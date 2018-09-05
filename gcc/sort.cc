/* Platform-independent deterministic sort function.
   Copyright (C) 2018 Free Software Foundation, Inc.
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

   The implementation uses a network sort for up to 5 elements and
   a merge sort on top of that.  Neither stage has branches depending on
   comparator result, trading extra arithmetic for branch mispredictions.  */

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

#include "system.h"

#define likely(cond) __builtin_expect ((cond), 1)

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
  size_t nlim; // limit for network sort
};

/* Helper for netsort. Permute, possibly in-place, 2 or 3 elements,
   placing E0 to C->OUT, E1 to C->OUT + C->SIZE, and so on.  */
static void
reorder23 (sort_ctx *c, char *e0, char *e1, char *e2)
{
#define REORDER_23(TYPE, STRIDE, OFFSET)                 \
do {                                                     \
  TYPE t0, t1;                                           \
  memcpy (&t0, e0 + OFFSET, sizeof (TYPE));              \
  memcpy (&t1, e1 + OFFSET, sizeof (TYPE));              \
  char *out = c->out + OFFSET;                           \
  if (likely (c->n == 3))                                \
    memmove (out + 2*STRIDE, e2 + OFFSET, sizeof (TYPE));\
  memcpy (out, &t0, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t1, sizeof (TYPE));                      \
} while (0)

  if (likely (c->size == sizeof (size_t)))
    REORDER_23 (size_t, sizeof (size_t), 0);
  else if (likely (c->size == sizeof (int)))
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
  if (likely (c->n == 5))                                \
    memmove (out + 4*STRIDE, e4 + OFFSET, sizeof (TYPE));\
  memcpy (out, &t0, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t1, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t2, sizeof (TYPE)); out += STRIDE;       \
  memcpy (out, &t3, sizeof (TYPE));                      \
} while (0)

  if (likely (c->size == sizeof (size_t)))
    REORDER_45 (size_t, sizeof (size_t), 0);
  else if (likely(c->size == sizeof (int)))
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
noinline static intptr_t
cmp1 (char *e0, char *e1, cmp_fn *cmp)
{
  intptr_t x = (intptr_t)e0 ^ (intptr_t)e1;
  return x & (cmp (e0, e1) >> 31);
}

/* Execute network sort on 2 to 5 elements from IN, placing them into C->OUT.
   IN may be equal to C->OUT, in which case elements are sorted in place.  */
static void
netsort (char *in, sort_ctx *c)
{
#define CMP(e0, e1)                   \
do {                                  \
  intptr_t x = cmp1 (e1, e0, c->cmp); \
  e0 = (char *)((intptr_t)e0 ^ x);    \
  e1 = (char *)((intptr_t)e1 ^ x);    \
} while (0)

  char *e0 = in, *e1 = e0 + c->size, *e2 = e1 + c->size;
  CMP (e0, e1);
  if (likely (c->n == 3))
    {
      CMP (e1, e2);
      CMP (e0, e1);
    }
  if (c->n <= 3)
    return reorder23 (c, e0, e1, e2);
  char *e3 = e2 + c->size, *e4 = e3 + c->size;
  if (likely (c->n == 5))
    {
      CMP (e3, e4);
      CMP (e2, e4);
    }
  CMP (e2, e3);
  if (likely (c->n == 5))
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
static void
mergesort (char *in, sort_ctx *c, size_t n, char *out, char *tmp)
{
  if (likely (n <= c->nlim))
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

  if (likely (c->cmp(r, l + (r - out) - c->size) < 0))
    {
      char *end = out + n * c->size;
      if (sizeof (size_t) == 8 && likely (c->size == 8))
	MERGE_ELTSIZE (8);
      else if (likely (c->size == 4))
	MERGE_ELTSIZE (4);
      else
	MERGE_ELTSIZE (c->size);
    }
  memcpy (out, l, r - out);
}

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
  qsort_chk (vbase, n, size, cmp);
#endif
}

void
gcc_stablesort (void *vbase, size_t n, size_t size, cmp_fn *cmp)
{
  gcc_qsort (vbase, n, ~size, cmp);
}
