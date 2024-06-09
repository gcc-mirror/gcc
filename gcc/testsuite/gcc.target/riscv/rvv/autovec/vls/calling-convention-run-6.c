/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

typedef long long v2df __attribute__ ((vector_size (16)));

v2df
add (v2df a1, v2df a2, v2df a3, v2df a4, v2df a5, v2df a6, v2df a7,
     v2df a8, v2df a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
}

int
main ()
{
  v2df a1 = {
    1.0, 1.0,
  };
  v2df a2 = {
    1.0, 1.0,
  };
  v2df a3 = {
    1.0, 1.0,
  };
  v2df a4 = {
    1.0, 1.0,
  };
  v2df a5 = {
    1.0, 1.0,
  };
  v2df a6 = {
    1.0, 1.0,
  };
  v2df a7 = {
    1.0, 1.0,
  };
  v2df a8 = {
    1.0, 1.0,
  };
  v2df a9 = {
    1.0, 1.0,
  };
  v2df expected = {
    9.0, 9.0,
  };
  v2df result = add (a1, a2, a3, a4, a5, a6, a7, a8, a9);

  unsigned i;

  for (i = 0; i < 2; i++)
    if (result[i] != expected[i])
      __builtin_abort ();

  return 0;
}
