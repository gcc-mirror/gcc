/* PR c/69669 */
/* { dg-do compile } */

enum __attribute__((mode(QI))) E { F = 1 };

void
foo (enum E *x, int y)
{
  *x = (enum E) y;
}
