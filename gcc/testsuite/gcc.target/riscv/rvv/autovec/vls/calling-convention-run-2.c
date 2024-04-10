/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

typedef short v8hi __attribute__ ((vector_size (16)));

v8hi
add (v8hi a1, v8hi a2, v8hi a3, v8hi a4, v8hi a5, v8hi a6, v8hi a7,
     v8hi a8, v8hi a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
}

int
main ()
{
  v8hi a1 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a2 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a3 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a4 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a5 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a6 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a7 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a8 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi a9 = {
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  v8hi expected = {
    9, 9, 9, 9, 9, 9, 9, 9,
  };
  v8hi result = add (a1, a2, a3, a4, a5, a6, a7, a8, a9);

  unsigned i;

  for (i = 0; i < 8; i++)
    if (result[i] != expected[i])
      __builtin_abort ();

  return 0;
}
