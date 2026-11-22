/* { dg-do run } */
/* { dg-options "-O2" } */

/* fabs, negation and -fabs of a value in memory, stored back in place,
   change only the sign bit.  */

#define SF 0x80000000u
#define SD 0x8000000000000000ull

__attribute__ ((noipa)) void abs_d (double *p) { *p = __builtin_fabs (*p); }
__attribute__ ((noipa)) void neg_d (double *p) { *p = -*p; }
__attribute__ ((noipa)) void abs_f (float *p) { *p = __builtin_fabsf (*p); }
__attribute__ ((noipa)) void neg_f (float *p) { *p = -*p; }
__attribute__ ((noipa)) void nabs_d (double *p) { *p = -__builtin_fabs (*p); }
__attribute__ ((noipa)) void nabs_f (float *p) { *p = -__builtin_fabsf (*p); }

/* Zeros, normals, a subnormal, infinities and quiet NaNs with payloads
   (no signaling NaNs: an x87 load would quiet them).  */
static const unsigned int vf[] = {
  0, SF, 0x3fc00000u, 0xbfc00000u, 0x00000001u, 0x80000001u,
  0x7f800000u, 0xff800000u, 0x7fc00123u, 0xffc00123u
};
static const unsigned long long vd[] = {
  0, SD, 0x3ff8000000000000ull, 0xbff8000000000000ull, 1, SD | 1,
  0x7ff0000000000000ull, 0xfff0000000000000ull, 0x7ff8000000000123ull,
  0xfff8000000000123ull
};

#define CHECK(fn, T, I, v, expect)				\
  for (unsigned n = 0; n < sizeof v / sizeof v[0]; n++)	\
    {								\
      T x;							\
      I b;							\
      __builtin_memcpy (&x, &v[n], sizeof x);			\
      fn (&x);							\
      __builtin_memcpy (&b, &x, sizeof b);			\
      if (b != (expect))					\
	__builtin_abort ();					\
    }

int
main ()
{
  CHECK (abs_f, float, unsigned int, vf, vf[n] & ~SF);
  CHECK (neg_f, float, unsigned int, vf, vf[n] ^ SF);
  CHECK (abs_d, double, unsigned long long, vd, vd[n] & ~SD);
  CHECK (neg_d, double, unsigned long long, vd, vd[n] ^ SD);
  CHECK (nabs_f, float, unsigned int, vf, vf[n] | SF);
  CHECK (nabs_d, double, unsigned long long, vd, vd[n] | SD);
  return 0;
}
