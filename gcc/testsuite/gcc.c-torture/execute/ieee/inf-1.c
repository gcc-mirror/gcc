extern void abort (void);

int main()
{
  float fi = __builtin_inff();
  double di = __builtin_inf();
  long double li = __builtin_infl();

  float fh = __builtin_huge_valf();
  double dh = __builtin_huge_val();
  long double lh = __builtin_huge_vall();

  if (fi + fi != fi)
    abort ();
  if (di + di != di)
    abort ();
  if (li + li != li)
    abort ();

  if (fi != fh)
    abort ();
  if (di != dh)
    abort ();
  if (li != lh)
    abort ();

  if (fi <= 0)
    abort ();
  if (di <= 0)
    abort ();
  if (li <= 0)
    abort ();

  return 0;
}
