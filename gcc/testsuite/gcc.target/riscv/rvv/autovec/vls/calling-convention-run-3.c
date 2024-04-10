/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

typedef int v4si __attribute__ ((vector_size (16)));

v4si
add (v4si a1, v4si a2, v4si a3, v4si a4, v4si a5, v4si a6, v4si a7,
     v4si a8, v4si a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
}

int
main ()
{
  v4si a1 = {
    1, 1, 1, 1,
  };
  v4si a2 = {
    1, 1, 1, 1,
  };
  v4si a3 = {
    1, 1, 1, 1,
  };
  v4si a4 = {
    1, 1, 1, 1,
  };
  v4si a5 = {
    1, 1, 1, 1,
  };
  v4si a6 = {
    1, 1, 1, 1,
  };
  v4si a7 = {
    1, 1, 1, 1,
  };
  v4si a8 = {
    1, 1, 1, 1,
  };
  v4si a9 = {
    1, 1, 1, 1,
  };
  v4si expected = {
    9, 9, 9, 9,
  };
  v4si result = add (a1, a2, a3, a4, a5, a6, a7, a8, a9);

  unsigned i;

  for (i = 0; i < 4; i++)
    if (result[i] != expected[i])
      __builtin_abort ();

  return 0;
}
