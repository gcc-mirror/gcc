/* PR c/69669 */
/* { dg-do compile } */
/* { dg-options "-fdump-rtl-final" } */

enum __attribute__((mode(QI))) E { F = 1 };

void
foo (enum E *x, int y)
{
  *x = (enum E) y;
}

/* { dg-final { scan-rtl-dump-times "mem:QI" 1 "final" } } */
