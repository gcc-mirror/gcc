/* { dg-do compile } */
/* { dg-options "-Wdangling-else" } */

void bar (int);
void
foo (int a, int b)
{
  if (a) /* { dg-warning "suggest explicit braces to avoid ambiguous" } */
    if (b)
      bar (1);
  else
    bar (2);
}
