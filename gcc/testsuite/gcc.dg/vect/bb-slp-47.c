/* { dg-do compile } */

int bar();
int foo (int *a, int b, int c)
{
  int tem0 = bar ();
  int tem1 = tem0 + b;
  int tem3 = tem1 + c;
  a[0] = tem3;
  a[1] = tem3 + 1;
  a[2] = tem3 + 2;
  a[3] = tem3 + 3;
  return tem1;
}
