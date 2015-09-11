/* { dg-lto-do link } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options { { -O2 -flto -flto-partition=max -fcheck-pointer-bounds -mmpx } } } */

int test1 (const char *c)
{
  return c[0] * 2;
}

int test2 (const char *c)
{
  return c[1] * 3;
}

int test1_alias (const char *c) __attribute__ ((alias ("test1")));
int test2_alias (const char *c) __attribute__ ((alias ("test2")));

struct S
{
  int (*fnptr[2]) (const char *);
} S;

struct S s = {test1_alias, test2_alias};

int main (int argc, const char **argv)
{
  return s.fnptr[argc] (argv[0]);
}
