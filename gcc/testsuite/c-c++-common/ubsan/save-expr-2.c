/* { dg-do compile } */
/* { dg-options "-fsanitize=shift -Wall -Werror -O" } */

int
foo (int i, unsigned int u)
{
  return u / i;
}

int
bar (int i, unsigned int u)
{
  return u % i;
}
