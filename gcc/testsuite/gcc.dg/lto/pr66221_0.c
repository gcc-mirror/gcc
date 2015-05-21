/* { dg-lto-do link } */
/* { dg-require-effective-target mpx } */
/* { dg-lto-options { { -O2 -flto -fcheck-pointer-bounds -mmpx } } } */

int test1 (const char *);

int main (int argc, const char **argv)
{
  return test1 (argv[0]);
}
