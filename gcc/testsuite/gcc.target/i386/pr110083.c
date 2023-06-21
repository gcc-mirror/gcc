/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4 -mstv -mno-stackrealign" } */
typedef int TItype __attribute__ ((mode (TI)));
typedef unsigned int UTItype __attribute__ ((mode (TI)));

void foo (void)
{
  static volatile TItype ivin, ivout;
  static volatile float fv1, fv2;
  ivin = ((TItype) (UTItype) ~ (((UTItype) ~ (UTItype) 0) >> 1));
  fv1 = ((TItype) (UTItype) ~ (((UTItype) ~ (UTItype) 0) >> 1));
  fv2 = ivin;
  ivout = fv2;
  if (ivin != ((TItype) (UTItype) ~ (((UTItype) ~ (UTItype) 0) >> 1))
      || ((((128) > sizeof (TItype) * 8 - 1)) && ivout != ivin)
      || ((((128) > sizeof (TItype) * 8 - 1))
	  && ivout !=
	  ((TItype) (UTItype) ~ (((UTItype) ~ (UTItype) 0) >> 1)))
      || fv1 !=
      (float) ((TItype) (UTItype) ~ (((UTItype) ~ (UTItype) 0) >> 1))
      || fv2 !=
      (float) ((TItype) (UTItype) ~ (((UTItype) ~ (UTItype) 0) >> 1))
      || fv1 != fv2)
    __builtin_abort ();
}

