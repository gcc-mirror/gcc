/* { dg-do compile } */
/* { dg-options "-O" } */

/* These contain undefined behavior but may trigger edge cases in the
   vector shift patterns.  We don't check for their generation, we only
   care about not ICEing.  */

typedef long long int Int64x1;
typedef int Int32x1;

#define force_simd_di(v) asm volatile ("mov %d0, %1.d[0]" : "=w"(v) : "w"(v) :)
#define force_simd_si(v) asm volatile ("mov %s0, %1.s[0]" : "=w"(v) : "w"(v) :)

Int64x1
foo_di (Int64x1 b)
{
  force_simd_di (b);
  b = b >> 63;
  force_simd_di (b);
  b = b >> 0;
  b += b >> 65; /* { dg-warning "right shift count >= width of type" } */

  return b;
}

Int32x1
foo_si (Int32x1 b)
{
  force_simd_si (b);
  b = b >> 31;
  force_simd_si (b);
  b = b >> 0;
  b += b >> 33; /* { dg-warning "right shift count >= width of type" } */

  return b;
}
