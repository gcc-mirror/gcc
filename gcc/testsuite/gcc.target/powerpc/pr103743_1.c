/* { dg-do run } */
/* { dg-options "-O2 -std=c99" } */

int
foo (int a)
{
  return a + 6;
}

int __attribute__ ((noipa)) udi_fun (unsigned long long in)
{
  if (in == (0x8642000000000000ULL))
    return foo (1);
  if (in == (0x7642000000000000ULL))
    return foo (12);
  if (in == (0x8000000000000000ULL))
    return foo (32);
  if (in == (0x8700000000000091ULL))
    return foo (33);
  if (in == (0x8642FFFFFFFFFFFFULL))
    return foo (46);
  if (in == (0x7642FFFFFFFFFFFFULL))
    return foo (51);
  if (in == (0x7567000000ULL))
    return foo (9);
  if (in == (0xFFF8567FFFFFFFFFULL))
    return foo (19);
  
  return 0;
}

int __attribute__ ((noipa)) di_fun (long long in)
{
  if (in == (0x8642000000000000LL))
    return foo (1);
  if (in == (0x7642000000000000LL))
    return foo (12);
  if (in == (0x8000000000000000LL))
    return foo (32);
  if (in == (0x8700000000000091LL))
    return foo (33);
  if (in == (0x8642FFFFFFFFFFFFLL))
    return foo (46);
  if (in == (0x7642FFFFFFFFFFFFLL))
    return foo (51);
  return 0;
}

int
main ()
{
  int e = 0;
  if (udi_fun (6) != 0)
    e++;
  if (udi_fun (0x8642000000000000ULL) != foo (1))
    e++;
  if (udi_fun (0x7642000000000000ULL) != foo (12))
    e++;
  if (udi_fun (0x8000000000000000ULL) != foo (32))
    e++;
  if (udi_fun (0x8700000000000091ULL) != foo (33))
    e++;
  if (udi_fun (0x8642FFFFFFFFFFFFULL) != foo (46))
    e++;
  if (udi_fun (0x7642FFFFFFFFFFFFULL) != foo (51))
    e++;
  if (udi_fun (0x7567000000ULL) != foo (9))
    e++;
  if (udi_fun (0xFFF8567FFFFFFFFFULL) != foo (19))
    e++;

  if (di_fun (6) != 0)
    e++;
  if (di_fun (0x8642000000000000LL) != foo (1))
    e++;
  if (di_fun (0x7642000000000000LL) != foo (12))
    e++;
  if (di_fun (0x8000000000000000LL) != foo (32))
    e++;
  if (di_fun (0x8700000000000091LL) != foo (33))
    e++;
  if (di_fun (0x8642FFFFFFFFFFFFLL) != foo (46))
    e++;
  if (di_fun (0x7642FFFFFFFFFFFFLL) != foo (51))
    e++;
  if (udi_fun (0x7567000000LL) != foo (9))
    e++;
  if (udi_fun (0xFFF8567FFFFFFFFFLL) != foo (19))
    e++;

  if (e)
    __builtin_abort ();
  return 0;
}

