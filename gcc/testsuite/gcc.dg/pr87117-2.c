/* { dg-do compile } */
/* { dg-options "-O -fcode-hoisting" } */

void e();

void a(int c, char **d)
{
  char b;
  if (1 < c)
    b = (char)(__INTPTR_TYPE__)d[0];
  if (1 < c && b)
    e();
  while (1 < c)
    ;
}
