/* PR rtl-optimization/65980 */
/* { dg-do compile } */
/* { dg-options "-O3 -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

typedef struct { int b; } A;
void (*a) (int);
int b;

int
foo (A *v)
{
  asm goto ("" : : "m" (v->b) : : l);
  return 0;
l:
  return 1;
}

int
bar (void)
{
  if (b)
    {
      if (foo (0) && a)
	a (0);
      return 0;
    }
  if (foo (0) && a)
    a (0);
  return 0;
}
