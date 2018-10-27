/* { dg-do compile } */

void f (void);
void g (void);
void h (int a)
{
  void *p, **q;
  if (a)
    p = (void *)f;
  else
    p = (void *)g;
  q = (void *)p;
  if (*q == (void *)0)
    goto *p;
L0:
  return;
}
