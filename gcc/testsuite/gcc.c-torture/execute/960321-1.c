char a[10] = "deadbeef";

char
acc_a (long i)
{
  return a[i-2000000000L];
}

main ()
{
  if (acc_a (2000000000L) != 'd')
    abort ();
  exit (0);
}
