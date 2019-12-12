/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

struct A
{
  int b[4];
};
struct B
{
  char a[12];
  int b;
};
struct C
{
  char a[16];
};

struct A
f1 (void)
{
  struct A x = {};
  return x;
}

struct B
f2 (void)
{
  struct B x = {};
  return x;
}

struct C
f3 (void)
{
  struct C x = {};
  return x;
}

/* { dg-final { scan-assembler-not "xmm" } } */
