/* { dg-do compile } */

void f (char *a, int i)
{
#pragma GCC novector
  for (;;i++)
    a[i] *= 2;
}


