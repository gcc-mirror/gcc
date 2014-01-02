/* A type-safe hash table template.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Lawrence Crowl <crowl@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This file implements a typed hash table.
   The implementation borrows from libiberty's hashtab.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hash-table.h"


/* Table of primes and multiplicative inverses.

   Note that these are not minimally reduced inverses.  Unlike when generating
   code to divide by a constant, we want to be able to use the same algorithm
   all the time.  All of these inverses (are implied to) have bit 32 set.

   For the record, here's the function that computed the table; it's a 
   vastly simplified version of the function of the same name from gcc.  */

#if 0
unsigned int
ceil_log2 (unsigned int x)
{
  int i;
  for (i = 31; i >= 0 ; --i)
    if (x > (1u << i))
      return i+1;
  abort ();
}

unsigned int
choose_multiplier (unsigned int d, unsigned int *mlp, unsigned char *shiftp)
{
  unsigned long long mhigh;
  double nx;
  int lgup, post_shift;
  int pow, pow2;
  int n = 32, precision = 32;

  lgup = ceil_log2 (d);
  pow = n + lgup;
  pow2 = n + lgup - precision;

  nx = ldexp (1.0, pow) + ldexp (1.0, pow2);
  mhigh = nx / d;

  *shiftp = lgup - 1;
  *mlp = mhigh;
  return mhigh >> 32;
}
#endif

struct prime_ent const prime_tab[] = {
  {          7, 0x24924925, 0x9999999b, 2 },
  {         13, 0x3b13b13c, 0x745d1747, 3 },
  {         31, 0x08421085, 0x1a7b9612, 4 },
  {         61, 0x0c9714fc, 0x15b1e5f8, 5 },
  {        127, 0x02040811, 0x0624dd30, 6 },
  {        251, 0x05197f7e, 0x073260a5, 7 },
  {        509, 0x01824366, 0x02864fc8, 8 },
  {       1021, 0x00c0906d, 0x014191f7, 9 },
  {       2039, 0x0121456f, 0x0161e69e, 10 },
  {       4093, 0x00300902, 0x00501908, 11 },
  {       8191, 0x00080041, 0x00180241, 12 },
  {      16381, 0x000c0091, 0x00140191, 13 },
  {      32749, 0x002605a5, 0x002a06e6, 14 },
  {      65521, 0x000f00e2, 0x00110122, 15 },
  {     131071, 0x00008001, 0x00018003, 16 },
  {     262139, 0x00014002, 0x0001c004, 17 },
  {     524287, 0x00002001, 0x00006001, 18 },
  {    1048573, 0x00003001, 0x00005001, 19 },
  {    2097143, 0x00004801, 0x00005801, 20 },
  {    4194301, 0x00000c01, 0x00001401, 21 },
  {    8388593, 0x00001e01, 0x00002201, 22 },
  {   16777213, 0x00000301, 0x00000501, 23 },
  {   33554393, 0x00001381, 0x00001481, 24 },
  {   67108859, 0x00000141, 0x000001c1, 25 },
  {  134217689, 0x000004e1, 0x00000521, 26 },
  {  268435399, 0x00000391, 0x000003b1, 27 },
  {  536870909, 0x00000019, 0x00000029, 28 },
  { 1073741789, 0x0000008d, 0x00000095, 29 },
  { 2147483647, 0x00000003, 0x00000007, 30 },
  /* Avoid "decimal constant so large it is unsigned" for 4294967291.  */
  { 0xfffffffb, 0x00000006, 0x00000008, 31 }
};

/* The following function returns an index into the above table of the
   nearest prime number which is greater than N, and near a power of two. */

unsigned int
hash_table_higher_prime_index (unsigned long n)
{
  unsigned int low = 0;
  unsigned int high = sizeof (prime_tab) / sizeof (prime_tab[0]);

  while (low != high)
    {
      unsigned int mid = low + (high - low) / 2;
      if (n > prime_tab[mid].prime)
	low = mid + 1;
      else
	high = mid;
    }

  /* If we've run out of primes, abort.  */
  if (n > prime_tab[low].prime)
    {
      fprintf (stderr, "Cannot find prime bigger than %lu\n", n);
      abort ();
    }

  return low;
}

/* Return X % Y using multiplicative inverse values INV and SHIFT.

   The multiplicative inverses computed above are for 32-bit types,
   and requires that we be able to compute a highpart multiply.

   FIX: I am not at all convinced that
     3 loads, 2 multiplications, 3 shifts, and 3 additions
   will be faster than
     1 load and 1 modulus
   on modern systems running a compiler.  */

#ifdef UNSIGNED_64BIT_TYPE
static inline hashval_t
mul_mod (hashval_t x, hashval_t y, hashval_t inv, int shift)
{
  __extension__ typedef UNSIGNED_64BIT_TYPE ull;
   hashval_t t1, t2, t3, t4, q, r;

   t1 = ((ull)x * inv) >> 32;
   t2 = x - t1;
   t3 = t2 >> 1;
   t4 = t1 + t3;
   q  = t4 >> shift;
   r  = x - (q * y);

   return r;
}
#endif

/* Compute the primary table index for HASH given current prime index.  */

hashval_t
hash_table_mod1 (hashval_t hash, unsigned int index)
{
  const struct prime_ent *p = &prime_tab[index];
#ifdef UNSIGNED_64BIT_TYPE
  if (sizeof (hashval_t) * CHAR_BIT <= 32)
    return mul_mod (hash, p->prime, p->inv, p->shift);
#endif
  return hash % p->prime;
}


/* Compute the secondary table index for HASH given current prime index.  */

hashval_t
hash_table_mod2 (hashval_t hash, unsigned int index)
{
  const struct prime_ent *p = &prime_tab[index];
#ifdef UNSIGNED_64BIT_TYPE
  if (sizeof (hashval_t) * CHAR_BIT <= 32)
    return 1 + mul_mod (hash, p->prime - 2, p->inv_m2, p->shift);
#endif
  return 1 + hash % (p->prime - 2);
}
