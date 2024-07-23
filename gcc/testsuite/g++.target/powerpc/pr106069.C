/* { dg-do run } */
/* { dg-options "-O -fno-tree-forwprop -maltivec" } */
/* { dg-require-effective-target vmx_hw } */

typedef __attribute__ ((altivec (vector__))) unsigned native_simd_type;

union
{
  native_simd_type V;
  int R[4];
} store_le_vec;

struct S
{
  S () = default;
  S (unsigned B0)
  {
    native_simd_type val{B0};
    m_simd = val;
  }
  void store_le (unsigned int out[])
  {
    store_le_vec.V = m_simd;
    unsigned int x0 = store_le_vec.R[0];
    __builtin_memcpy (out, &x0, 4);
  }
  S rotl (unsigned int r)
  {
    native_simd_type rot{r};
    return __builtin_vec_rl (m_simd, rot);
  }
  void operator+= (S other)
  {
    m_simd = __builtin_vec_add (m_simd, other.m_simd);
  }
  void operator^= (S other)
  {
    m_simd = __builtin_vec_xor (m_simd, other.m_simd);
  }
  static void transpose (S &B0, S B1, S B2, S B3)
  {
    native_simd_type T0 = __builtin_vec_mergeh (B0.m_simd, B2.m_simd);
    native_simd_type T1 = __builtin_vec_mergeh (B1.m_simd, B3.m_simd);
    native_simd_type T2 = __builtin_vec_mergel (B0.m_simd, B2.m_simd);
    native_simd_type T3 = __builtin_vec_mergel (B1.m_simd, B3.m_simd);
    B0 = __builtin_vec_mergeh (T0, T1);
    B3 = __builtin_vec_mergel (T2, T3);
  }
  S (native_simd_type x) : m_simd (x) {}
  native_simd_type m_simd;
};

void
foo (unsigned int output[], unsigned state[])
{
  S R00 = state[0];
  S R01 = state[0];
  S R02 = state[2];
  S R03 = state[0];
  S R05 = state[5];
  S R06 = state[6];
  S R07 = state[7];
  S R08 = state[8];
  S R09 = state[9];
  S R10 = state[10];
  S R11 = state[11];
  S R12 = state[12];
  S R13 = state[13];
  S R14 = state[4];
  S R15 = state[15];
  for (int r = 0; r != 10; ++r)
    {
      R09 += R13;
      R11 += R15;
      R05 ^= R09;
      R06 ^= R10;
      R07 ^= R11;
      R07 = R07.rotl (7);
      R00 += R05;
      R01 += R06;
      R02 += R07;
      R15 ^= R00;
      R12 ^= R01;
      R13 ^= R02;
      R00 += R05;
      R01 += R06;
      R02 += R07;
      R15 ^= R00;
      R12 = R12.rotl (8);
      R13 = R13.rotl (8);
      R10 += R15;
      R11 += R12;
      R08 += R13;
      R09 += R14;
      R05 ^= R10;
      R06 ^= R11;
      R07 ^= R08;
      R05 = R05.rotl (7);
      R06 = R06.rotl (7);
      R07 = R07.rotl (7);
    }
  R00 += state[0];
  S::transpose (R00, R01, R02, R03);
  R00.store_le (output);
}

unsigned int res[1];
unsigned main_state[]{1634760805, 60878,      2036477234, 6,
		      0,	  825562964,  1471091955, 1346092787,
		      506976774,  4197066702, 518848283,  118491664,
		      0,	  0,	      0,	  0};
int
main ()
{
  foo (res, main_state);
  if (res[0] != 0x41fcef98)
    __builtin_abort ();
  return 0;
}
