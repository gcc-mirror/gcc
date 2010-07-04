/* { dg-options "-Wunused" } */
/* { dg-do compile } */

int f1 (int *, int);
int f2 (int *);
int f3 (int *);

int
f4 (int x)
{
  int a, n = 0;
  int b;
  for (a = f1 (&b, x); f2 (&b); (void) (a = f3 (&b)))
    n++;
  return n;
}

void
f5 (int x)
{
  int a;
  a = x;
  (void) (a = x);
}

void
f6 (int x)
{
  int a;	/* { dg-warning "set but not used" } */
  a = x;
}

void
f7 (int x)
{
  int a;
  ({ a = x; });
}

int
f8 (int x)
{
  int a;
  int b = ({ a = x; });
  return b;
}

int v;

void
f9 (int x)
{
  int a;
  ({ v++, a = x; });
}

int
f10 (int x)
{
  int a;
  int b = ({ v++, a = x; });
  return b;
}

void
f11 (int x)
{
  int a;
  a = x;
  ({ v++, a; });
}

int
f12 (int x)
{
  int a;
  a = x;
  int b = ({ v++, a; });
  return b;
}
