/* { dg-do run } */
/* { dg-options "-g -fno-ipa-icf" } */

volatile int v;

int __attribute__((noipa))
get_val0 (void)  {return 0;}
int __attribute__((noipa))
get_val2 (void)  {return 2;}

struct S
{
  int a, b, c;
};

static int __attribute__((noinline))
bar (struct S s, int x, int y, int z, int i)
{
  int r;
  v = s.a + s.b;		/* { dg-final { gdb-test . "i+1" "3" } } */
  return r;
}

static int __attribute__((noinline))
foo (struct S s, int i)
{
  int r;
  r = bar (s, 3, 4, 5, i);
  return r;
}


int
main (void)
{
  struct S s;
  int i;
  i = get_val2 ();
  s.a = get_val0 ();
  s.b = get_val0 ();
  s.c = get_val0 ();
  int r = foo (s, i);
  v = r + i;
  return 0;
}
