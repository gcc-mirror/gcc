/* { dg-lto-do run } */
/* { dg-extra-ld-options { -funsigned-char } } */

char n[3] = {'a','b','c'};
int foo(char *x)
{
  if (*x == 'b')
    return (int)*x;
  *x = 'y';
  return 0;
}
