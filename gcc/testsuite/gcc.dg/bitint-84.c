/* PR tree-optimization/113737 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23" } */

#if __BITINT_MAXWIDTH__ >= 129
_BitInt(129) a;
#else
_BitInt(63) a;
#endif

int b[1], c;

int
foo (void)
{
  switch (a)
  case 0:
  case 2:
    return 1;
  return 0;
}

void
bar (int i)
{
  for (;; ++i)
    {
      c = b[i];
      if (!foo ())
	__asm__ ("");
    }
}
