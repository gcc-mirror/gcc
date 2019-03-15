/* PR debug/89704 */
/* { dg-do compile } */

typedef __INTPTR_TYPE__ intptr_t;

int
foo (void)
{
  lab1:;
  lab2:;
  static int i = (intptr_t) &&lab1 - (intptr_t) &&lab2;
  static int j = (intptr_t) &&lab1 - (intptr_t) &&lab2;
  return i;
}
