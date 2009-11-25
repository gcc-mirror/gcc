/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler-not "cmp" } } */

int bar();
void goo(int, int);

void eq()
{
  int v = bar();
  if (v == 0)
    return;
  goo(1, v);
}

void ge()
{
  int v = bar();
  if (v >= 0)
    return;
  goo(1, v);
}

void gt()
{
  int v = bar();
  if (v > 0)
    return;
  goo(1, v);
}

void lt()
{
  int v = bar();
  if (v < 0)
    return;
  goo(1, v);
}

void le()
{
  int v = bar();
  if (v <= 0)
    return;
  goo(1, v);
}

unsigned int foo();

void leu()
{
  unsigned int v = foo();
  if (v <= 0)
    return;
  goo(1, v);
}
