/* Positive test for uninitialized variables.  */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

int sink;

void f1(int parm)	/* { dg-bogus "uninitialized" "parameter" } */
{
  sink = parm;		/* { dg-bogus "uninitialized" "parameter" } */
}

void f2(void)
{
  int x;
  sink = x;		/* { dg-warning "is used" "unconditional" } */
}

void f3(int p)
{
  int x;		
  if (p)
    x = p;
  sink = x;            /* { dg-warning "may be used" "conditional" } */
}

void f4(int p)
{
  int x;		/* { dg-bogus "uninitialized" "easy if" } */
  if (p)
    x = 1;
  else
    x = 2;
  sink = x;
}

void f5(void)
{
  int x, i;		/* { dg-bogus "uninitialized" "easy loop" } */
  for (i = 0; i < 10; ++i)
    x = 1;
  sink = x;
}
