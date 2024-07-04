/* { dg-do compile } */
/* { dg-additional-options "-mcpu=neoverse-v1" { target aarch64*-*-* } } */

char *a;
int b;
void c()
{
  int d = 0, e = 0, f;
  for (; f; ++f)
    if (a[f] == 5)
      ;
    else if (a[f])
      e = 1;
    else
      d = 1;
  if (d)
    if (e)
      b = 0;
}
