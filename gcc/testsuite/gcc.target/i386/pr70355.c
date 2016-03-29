/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -fno-tree-ter -funroll-loops -mavx512f -g" } */

typedef unsigned __int128 v2ti __attribute__ ((vector_size (32)));

unsigned
foo (unsigned i, v2ti v)
{
  do {
    i--;
    v %= ~v;
  } while (i);
  return v[0] + v[1];
}
