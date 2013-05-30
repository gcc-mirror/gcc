/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

extern int * alloca (int);

int *p;

void
test (int a)
{
  if (a > 0)
    p = alloca (4);
}
