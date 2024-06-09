/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
/* { dg-require-effective-target trampolines } */
/* { dg-additional-options "-std=gnu89" } */

f(n)
{
  int a[n];
  int g(i)
    {
      return a[i];
    }
  a[1]=4711;
  return g(1);
}
main()
{
  if(f(2)!=4711)abort();

  exit(0);
}
