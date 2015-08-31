/* { dg-do compile } */

int a;
void
f (void)
{
  if (!a);
  else
  lbl:
    a = a;

  if (a)
    a = 8;
  goto lbl;
}
