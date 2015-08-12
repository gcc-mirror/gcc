/* { dg-lto-do link } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options { { -Ofast -flto -fcheck-pointer-bounds -mmpx } } } */

extern int __attribute__((noinline)) f1 (int i);

static int __attribute__((noinline))
f2 (int i)
{
  return i + 6;
}

int
main (int argc, char **argv)
{
  return f1 (argc) + f2 (argc);
}
