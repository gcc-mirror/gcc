/* { dg-do run } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-options "-O2 -march=z14 -mzarch" } */

__attribute__ ((noipa))
long long trunctf (long double x)
{
  /* Ensure via ++x that x is in a register.  */
  ++x;
  return x;
}

__attribute__ ((noipa))
long long trunctf_from_mem (long double x)
{
  return x;
}

int main (void)
{
  if (trunctf (0x7ffffffffffffffeLL) != 0x7fffffffffffffffLL)
    __builtin_abort ();
  if (trunctf_from_mem (0x7fffffffffffffffLL) != 0x7fffffffffffffffLL)
    __builtin_abort ();
  return 0;
}
