int b=1;
main()
{
  int a;
  int c;
  a=0xff;
  for (;b;b--)
  {
    c=1;
    asm(""::"r"(c));
    c=(char)a;
  }
  if (c!=-1)
    abort();
  return c;
}
