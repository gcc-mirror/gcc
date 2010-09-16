/* { dg-do compile } */
/* { dg-options "-O2 -m8bit-idiv" } */

extern void foo (unsigned int, unsigned int, unsigned int,
		 unsigned int, unsigned int, unsigned int);

void
bar (unsigned int x, unsigned int y)
{
  foo (0, 0, 0, 0, x / y, x % y);
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "divl" 1 } } */
