/* PR target/64897 */
/* { dg-do run } */
/* { dg-require-effective-target double64 } */
/* { dg-skip-if "not IEEE" { vax-*-* pdp11-*-* } } */
/* { dg-add-options ieee } */

#define F(name, T, I, OP, C)			\
  __attribute__((noipa)) T name (T x)		\
  {						\
    I i;					\
    __builtin_memcpy (&i, &x, sizeof i);	\
    i OP C;					\
    __builtin_memcpy (&x, &i, sizeof x);	\
    return x;					\
  }

#define SF 0x80000000u
#define SD 0x8000000000000000ull

F (abs_f, float, __UINT32_TYPE__, &=, ~SF)
F (abs_d, double, __UINT64_TYPE__, &=, ~SD)
F (neg_f, float, __UINT32_TYPE__, ^=, SF)
F (neg_d, double, __UINT64_TYPE__, ^=, SD)
F (nabs_f, float, __UINT32_TYPE__, |=, SF)
F (nabs_d, double, __UINT64_TYPE__, |=, SD)

/* Zeros, normals, a subnormal, infinities and quiet NaNs with payloads
   (no signaling NaNs: the ia32 ABI returns them quieted).  */
static const __UINT32_TYPE__ vf[] = {
  0, SF, 0x3fc00000u, 0xbfc00000u, 0x00000001u, 0x80000001u,
  0x7f800000u, 0xff800000u, 0x7fc00123u, 0xffc00123u
};
static const __UINT64_TYPE__ vd[] = {
  0, SD, 0x3ff8000000000000ull, 0xbff8000000000000ull, 1, SD | 1,
  0x7ff0000000000000ull, 0xfff0000000000000ull, 0x7ff8000000000123ull,
  0xfff8000000000123ull
};

#define CHECK(fn, T, I, v, expect)				\
  for (unsigned n = 0; n < sizeof v / sizeof v[0]; n++)	\
    {								\
      T x, r;							\
      I b;							\
      __builtin_memcpy (&x, &v[n], sizeof x);			\
      r = fn (x);						\
      __builtin_memcpy (&b, &r, sizeof b);			\
      if (b != (expect))					\
	__builtin_abort ();					\
    }

int
main ()
{
  CHECK (abs_f, float, __UINT32_TYPE__, vf, vf[n] & ~SF);
  CHECK (neg_f, float, __UINT32_TYPE__, vf, vf[n] ^ SF);
  CHECK (abs_d, double, __UINT64_TYPE__, vd, vd[n] & ~SD);
  CHECK (neg_d, double, __UINT64_TYPE__, vd, vd[n] ^ SD);
  CHECK (nabs_f, float, __UINT32_TYPE__, vf, vf[n] | SF);
  CHECK (nabs_d, double, __UINT64_TYPE__, vd, vd[n] | SD);
  return 0;
}
