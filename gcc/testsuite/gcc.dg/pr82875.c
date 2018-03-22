/* PR middle-end/82875 */
/* { dg-do compile } */
/* { dg-options "-ftree-ter" } */
/* { dg-require-effective-target alloca } */

const int a = 100;

void
foo (void)
{
  int c[a];
}
