/* { dg-do compile } */
/* { dg-options "-O2 -march=v10" } */
/* { dg-final { scan-assembler-not {\tnop} } } */
/* { dg-final { scan-assembler-times {\tcmp|\ttest|\tmove.d \$r10,\$r} 1 } } */

/* We either have a move from "a" to some other register or a compare. */

extern void foo(void);
unsigned int x (unsigned int a, unsigned int b, unsigned int *c, unsigned int *d)
{
  unsigned int z = __builtin_clz(b);
  if (a != 0)
    *c = a;
  *d = a;
  return z;
}
