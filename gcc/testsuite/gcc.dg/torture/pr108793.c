/* { dg-do compile } */

typedef int *p;
extern p a[], b[];
int f () {
  int n = 0;
  for (p* i = &a[0]; i > &b[0]; i++)
    n++;
  return n;
}
