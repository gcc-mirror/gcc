foo (a, b)
{
  return (a & (1 << b)) != 0;
}

bar (a, b)
{
  a ^= (1 << b);
  return a != 0;
}

main ()
{
  int i;
  for (i = 0; i < 32; i++)
    printf ("%d ", foo (0x8000000f, i));
  puts ("");
}
