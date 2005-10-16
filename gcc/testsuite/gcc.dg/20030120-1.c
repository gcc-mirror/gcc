/* PR 7154 */
/* { dg-do compile { target fpic } } */
/* { dg-options "-O -fpic" } */

const int x[1]={ 1 };
void foo(int i, int *p)
{
  asm volatile("" : "+r"(i) : "m" (x[0]), "r"(p));
}
