/* { dg-do run } */
/* { dg-options "-O3" } */

typedef unsigned int myint __attribute__((aligned(1)));

typedef struct S {
  unsigned a, b, c;
} SS;

typedef struct U {
  SS s[2];
} UU;

typedef UU __attribute__((aligned(1))) UUU;

static unsigned int __attribute__ ((noinline))
get_a (SS s)
{
  return s.a;
};

static int __attribute__ ((noinline, noclone))
foo (UUU *p)
{
  int r = (int) get_a(p->s[0]) + 2;
  return r;
}

char buf[512];

static UUU * __attribute__ ((noinline, noclone))
get_uuu (void)
{
  return (UUU *)(buf + 1);
}

int
main(int argc, char *argv[])
{
  UUU *p = get_uuu();
  if (foo(p) != 2)
    __builtin_abort ();
  return 0;
}
