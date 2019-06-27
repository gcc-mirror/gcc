/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -fno-tree-loop-distribute-patterns" } */

/* Note that it's required to disable loop-distribute-patterns, otherwise the
   loop will be optimized to memset.  */

/* Expect loop_iv can know the loop is finite so the doloop_optimize
   can perform the doloop transformation.  */

typedef struct {
  int l;
  int b[258];
} S;

void clear (S* s )
{
  int i;
  int len = s->l + 1;

  for (i = 0; i <= len; i++)
    s->b[i] = 0;
}

/* { dg-final { scan-assembler {\mbdnz\M} } } */
