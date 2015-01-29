/* { dg-do run } */

struct S
{
  int f1;
};

static struct S a = { 1 };
char b;
static unsigned char *c = &b;
int d, e, f;

int
fn1 (int p)
{
  return 0 ? 0 : p - 1;
}

static int
fn2 (struct S p)
{
  int g = 200;
  for (e = 4; e; e = fn1 (e))
    {
      for (; d; d++)
	;
      *c &= p.f1 & g;
      g = --*c;
      if (f)
	return 0;
    }
  return 0;
}

int
main ()
{
  fn2 (a);

  if (b != 0) 
    __builtin_abort (); 

  return 0;
}
