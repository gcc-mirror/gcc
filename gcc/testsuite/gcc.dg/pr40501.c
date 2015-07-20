/* { dg-do compile { target alpha*-*-* ia64*-*-* i?86-*-* x86_64-*-* s390x-*-* } } */
/* { dg-options "-O2 -Wno-shift-overflow" } */
/* { dg-require-effective-target lp64 } */

/* PR middle-end/40501 */

/* This once failed due to the bswap pass missing to add the type
   casts of the signed argument and result to the proper unsigned
   types.  */

typedef long int int64_t;

int64_t
swap64 (int64_t n)
{
  return (((n & (((int64_t) 0xff) )) << 56) |
	  ((n & (((int64_t) 0xff) << 8)) << 40) |
	  ((n & (((int64_t) 0xff) << 16)) << 24) |
	  ((n & (((int64_t) 0xff) << 24)) << 8) |
	  ((n & (((int64_t) 0xff) << 32)) >> 8) |
	  ((n & (((int64_t) 0xff) << 40)) >> 24) |
	  ((n & (((int64_t) 0xff) << 48)) >> 40) |
	  ((n & (((int64_t) 0xff) << 56)) >> 56));
}
