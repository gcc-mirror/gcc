foo (p)
     int *p;
{
  return (*p & 255) == 0;
}

bar (a)
{
  return (a & 0xfff00000) == 0;
}

main ()
{
  printf ("%d%d\n", bar (-1), bar(0));
}
