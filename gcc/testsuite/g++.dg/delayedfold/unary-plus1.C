// PR c++/70153
// { dg-do run }

unsigned long long int
fn1 (unsigned long long int i)
{
  return 2ULL * ((1 + (unsigned long int) +1) * i);
}

unsigned long long int
fn2 (unsigned long long int i)
{
  return 2ULL * ((1 + (unsigned long int) +(1 + 0)) * i);
}

int
main (void)
{
  if (fn1 (3ULL) != 12ULL
      || fn2 (3ULL) != 12ULL)
    __builtin_abort ();
}
