f(int *x)
{
  *x = 0;
}

main()
{
  int s, c, x;
  char a[] = "c";

  f(&s);
  a[c = 0] = s == 0 ? (x=1, 'a') : (x=2, 'b');
  if (a[c] != 'a')
    abort();
  exit (0);
}
