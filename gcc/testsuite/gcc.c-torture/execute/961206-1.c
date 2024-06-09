void abort (void);
void exit (int);

int
sub1 (unsigned long long i)
{
  if (i < 0x80000000)
    return 1;
  else
    return 0;
}

int
sub2 (unsigned long long i)
{
  if (i <= 0x7FFFFFFF)
    return 1;
  else
    return 0;
}

int
sub3 (unsigned long long i)
{
  if (i >= 0x80000000)
    return 0;
  else
    return 1;
}

int
sub4 (unsigned long long i)
{
  if (i > 0x7FFFFFFF)
    return 0;
  else
    return 1;
}

int
main(void)
{
  if (sub1 (0x80000000ULL))
    abort ();

  if (sub2 (0x80000000ULL))
    abort ();

  if (sub3 (0x80000000ULL))
    abort ();

  if (sub4 (0x80000000ULL))
    abort ();

  exit (0);
}
