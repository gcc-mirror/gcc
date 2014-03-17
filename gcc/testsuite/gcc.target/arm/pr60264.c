/* { dg-do compile } */
/* { dg-options "-mapcs -g" } */

double bar(void);

int foo(void)
{
  int i = bar() + bar();

  return i;
}

