/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-strong -fPIC" } */
/* { dg-require-effective-target fpic } */

void g(int*);
void
f (int x)
{
  int arr[10];
  g (arr);
}

