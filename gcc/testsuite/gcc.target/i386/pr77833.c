/* { dg-do compile } */
/* { dg-options "-O -mavx512f" } */

typedef unsigned long V __attribute__((vector_size(64)));
typedef unsigned __int128 W __attribute__((vector_size(64)));

V
foo(int i, V v)
{
  i *= ((W)(V){0, 0, 0, 0, 0, 1, v[0]})[2];
  v[i] = 0;
  i--;
  return v + i;
}
