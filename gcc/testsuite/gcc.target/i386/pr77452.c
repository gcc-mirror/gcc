/* { dg-do compile { target int128 } } */
/* { dg-options "-O -mavx512f -fno-split-wide-types --param max-combine-insns=2" } */

typedef unsigned      int U __attribute__((vector_size(64)));
typedef unsigned __int128 V __attribute__((vector_size(64)));

V
foo(V v)
{
  v[0] = 1u << ((  ((V)(U){1, 1, v[0]})[0]) & 0xf)
            >> ((-~((V)(U){1, 1, v[0]})[0]) & 0xf);
  return v;
}
