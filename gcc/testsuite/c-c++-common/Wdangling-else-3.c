/* { dg-do compile } */
/* { dg-options "-Wparentheses -Wno-dangling-else" } */

void bar (int);
void
foo (int a, int b)
{
  if (a) /* { dg-bogus "suggest explicit braces to avoid ambiguous" } */
    if (b)
      bar (1);
  else
    bar (2);
}
