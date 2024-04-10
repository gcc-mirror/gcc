/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

typedef long long v2di __attribute__ ((vector_size (16)));

v2di
add (v2di a1, v2di a2, v2di a3, v2di a4, v2di a5, v2di a6, v2di a7,
     v2di a8, v2di a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
}

int
main ()
{
  v2di a1 = {
    1, 1,
  };
  v2di a2 = {
    1, 1,
  };
  v2di a3 = {
    1, 1,
  };
  v2di a4 = {
    1, 1,
  };
  v2di a5 = {
    1, 1,
  };
  v2di a6 = {
    1, 1,
  };
  v2di a7 = {
    1, 1,
  };
  v2di a8 = {
    1, 1,
  };
  v2di a9 = {
    1, 1,
  };
  v2di expected = {
    9, 9,
  };
  v2di result = add (a1, a2, a3, a4, a5, a6, a7, a8, a9);

  unsigned i;

  for (i = 0; i < 2; i++)
    if (result[i] != expected[i])
      __builtin_abort ();

  return 0;
}
