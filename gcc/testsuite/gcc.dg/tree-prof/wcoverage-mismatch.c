/* { dg-options "-O2 -Wcoverage-mismatch -w" } */

int __attribute__((noinline)) bar (void)
{
}

int foo (int i)
{
#ifdef _PROFILE_USE
  if (i)
    bar ();
#endif
  return 0;
}

int main(int argc, char **argv)
{
  foo (argc);
  return 0;
}
