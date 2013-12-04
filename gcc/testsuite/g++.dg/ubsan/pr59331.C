/* { dg-do compile } */
/* { dg-options "-fsanitize=vla-bound -Wall -Wno-unused-variable" } */

void foo(int i)
{
  /* Don't warn here with "value computed is not used".  */
  char a[i];
}
