foo (a)
     int a;
{
  int b = 8;

  if ((a << b) >= 0)
    return 1;
  return a;
}

main ()
{
  if (foo (0x00ffffff) == 1)
    puts ("y");
}
