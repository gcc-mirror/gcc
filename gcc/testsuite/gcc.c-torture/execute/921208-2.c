/* { dg-require-effective-target untyped_assembly } */
/* { dg-require-stack-size "100000 * 4 + 1024" } */

g(){}

f()
{
  int i;
  float a[100000];

  for (i = 0; i < 1; i++)
    {
      g(1.0, 1.0 + i / 2.0 * 3.0);
      g(2.0, 1.0 + i / 2.0 * 3.0);
    }
}

main ()
{
  f();
  exit(0);
}
