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
#ifndef NO_TRAMPOLINES
  if(f(2)!=4711)abort();
#endif
  exit(0);
}
