/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-fre -fno-tree-sra -fdump-tree-optimized-slim"  } */

struct S
{
  int a, b, c;
};

volatile int z1;
int z2 = 44;

void  __attribute__((noipa))
use_int (int c)
{
  z1 = c;
}

static void __attribute__ ((noinline))
bar (struct S s)
{
  use_int (s.c);
}


static void __attribute__ ((noinline))
foo (struct S s)
{
  int c = s.c;
  if (c != 21)
    use_int (c);

  s.c = z2;
  bar (s);
  if (s.c != 44)
    __builtin_abort ();
}

int
main (void)
{
  struct S s;
  s.a = 1;
  s.b = 64;
  s.c = 21;
  foo (s);
  return 0;
}




/* { dg-final { scan-tree-dump-not "ellide" "optimized" } } */
