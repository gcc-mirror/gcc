/* PR target/95355 */
/* { dg-do assemble { target avx512dq } } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O -fno-tree-dominator-opts -fno-tree-fre -ftree-slp-vectorize -fno-tree-ter -mavx512dq -masm=intel" } */

typedef int __attribute__((__vector_size__(64))) U;
typedef __int128 __attribute__((__vector_size__(32))) V;

U i;
V j;

int
foo(unsigned char l)
{
  V m = j % 999;
  U n = l <= i;
  V o = ((union { U a; V b[2]; }) n).b[0] + m;
  return o[0];
}
