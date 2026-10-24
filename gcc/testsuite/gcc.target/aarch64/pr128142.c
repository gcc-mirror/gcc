/* PR tree-optimization/128142 */
/* With -fwrapv, (x + y) >> 1 is an average only if x + y cannot wrap.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv" } */

typedef int v4si __attribute__ ((vector_size (16)));
typedef unsigned int v4usi __attribute__ ((vector_size (16)));

/* Signed with one leading zero bit: the sum can wrap.  */
v4si
f1 (v4si a, v4si b)
{
  return ((a & 0x7fffffff) + (b & 0x7fffffff)) >> 1;
}

/* Signed with two leading zero bits: the sum cannot wrap.  */
v4si
f2 (v4si a, v4si b)
{
  return ((a & 0x3fffffff) + (b & 0x3fffffff)) >> 1;
}

/* Unsigned with one leading zero bit: the sum cannot wrap.  */
v4usi
f3 (v4usi a, v4usi b)
{
  return ((a & 0x7fffffff) + (b & 0x7fffffff)) >> 1;
}

/* { dg-final { scan-assembler-times {\tshadd\tv[0-9]+\.4s,} 1 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.4s,} 1 } } */
