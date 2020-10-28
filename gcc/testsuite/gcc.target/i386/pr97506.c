/* PR target/97506  */
/* { dg-do compile } */
/* { dg-options "-Og -finline-functions-called-once -fno-tree-ccp -mavx512vbmi -mavx512vl" } */

typedef unsigned char __attribute__ ((__vector_size__ (16))) U;
typedef int __attribute__ ((__vector_size__ (4))) V;
U u;

void
bar (int i, V v)
{
  u += (char) i & (char) i > (U){};
}

void
foo (void)
{
  bar (0, (V){});
}
