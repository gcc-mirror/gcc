/* PR target/65520 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo (void *);

void
bar (void)
{
  unsigned s = 128;
  while (1)
    {
      unsigned b[s];
      if (foo (b))
	break;
      s *= 2;
    }
}

/* { dg-final { scan-assembler-not "div\[^\n\r]*%" } } */
