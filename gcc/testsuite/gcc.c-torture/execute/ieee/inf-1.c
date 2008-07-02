extern void abort (void);

int main()
{
#ifndef __SPU__
  /* The SPU single-precision floating point format does not support Inf.  */
  float fi = __builtin_inff();
#endif
  double di = __builtin_inf();
  long double li = __builtin_infl();

  float fh = __builtin_huge_valf();
  double dh = __builtin_huge_val();
  long double lh = __builtin_huge_vall();

#ifndef __SPU__
  if (fi + fi != fi)
    abort ();
#endif
  if (di + di != di)
    abort ();
  if (li + li != li)
    abort ();

#ifndef __SPU__
  if (fi != fh)
    abort ();
#endif
  if (di != dh)
    abort ();
  if (li != lh)
    abort ();

#ifndef __SPU__
  if (fi <= 0)
    abort ();
#endif
  if (di <= 0)
    abort ();
  if (li <= 0)
    abort ();

  return 0;
}
