f()
{
  int x = 1;
#if defined(STACK_SIZE)
  char big[STACK_SIZE/2];
#else
  char big[0x1000];
#endif

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
