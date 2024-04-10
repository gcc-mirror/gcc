/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

typedef float v4sf __attribute__ ((vector_size (16)));

v4sf
add (v4sf a1, v4sf a2, v4sf a3, v4sf a4, v4sf a5, v4sf a6, v4sf a7,
     v4sf a8, v4sf a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
}

int
main ()
{
  v4sf a1 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a2 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a3 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a4 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a5 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a6 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a7 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a8 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf a9 = {
    1.0, 1.0, 1.0, 1.0,
  };
  v4sf expected = {
    9.0, 9.0, 9.0, 9.0,
  };
  v4sf result = add (a1, a2, a3, a4, a5, a6, a7, a8, a9);

  unsigned i;

  for (i = 0; i < 4; i++)
    if (result[i] != expected[i])
      __builtin_abort ();

  return 0;
}
