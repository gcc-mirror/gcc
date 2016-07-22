/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O1" } */

struct S
{
  int f;
};

double a;
int c;

static
void fn1 (struct S *p1)
{
  for (; c; )
    if (p1->f++)
      a = (int) p1;
}

int
main ()
{
  struct S b = { 0 };
  fn1 (&b);
  return 0;
}
