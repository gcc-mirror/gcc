long long
foo (a)
     int a;
{
  return a;
}

main ()
{
  printf ("%d\n", (int) (foo (-1) >> 32));
}
