f()
{
  int x = 1;
  char big[0x1000];

  ({
    __label__ mylabel;
  mylabel:
    x++;
    if (x != 3)
      goto mylabel;
  });
  exit(0);
}

main()
{
  f();
}
