void abort (void);
void exit (int);

int
ts(a)
     int a;
{
  if (a < 1000 && a > 2000)
    return 1;
  else
    return 0;
}

int
tu(a)
     unsigned int a;
{
  if (a < 1000 && a > 2000)
    return 1;
  else
    return 0;
}

int
main(void)
{
  if (ts (0) || tu (0))
    abort ();
  exit (0);
}
