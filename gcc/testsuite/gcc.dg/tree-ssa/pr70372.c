/* { dg-do compile } */
/* { dg-options "-O -fno-tree-fre -w -Wno-psabi" } */

typedef unsigned v2ti __attribute__ ((vector_size (32)));

v2ti
foo (v2ti u, v2ti v)
{
  u[0] >>= 0xf;
  v ^= ~v;
  v &= ~u;
  v -= -u;
  return v;
}
