/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-add-options vect_early_break } */

char a;

void
b (void)
{
  &&c;
  void *e = &&d;
  goto *e;
f:
  a = a + 1;
  goto g;
h:
  goto f;
g:
  if (4U + a)
  d:
  c:
    goto h;
}
