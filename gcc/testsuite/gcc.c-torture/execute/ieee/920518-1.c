unsigned u=2147483839;float f0=2147483648e0,f1=2147483904e0;
main()
{
  float f=u;
  if(f==f0)
    abort();
  exit(0);
}
