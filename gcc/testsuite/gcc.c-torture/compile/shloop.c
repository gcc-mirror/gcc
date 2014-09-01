main ()
{
  int volatile p;
  int i;
  for (i = 10000000; i > 0; i--)
    p = i >> 10;
}
