/* Public domain.  */
#if __LDBL_MANT_DIG__ == 106 || __LDBL_MANT_DIG__ == 113
typedef int DItype __attribute__ ((mode (DI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef float TFtype __attribute__ ((mode (TF)));

DItype __fixunstfdi (TFtype);

DItype
__fixunstfdi (TFtype a)
{
  if (a < 0)
    return 0;

  /* Compute high word of result, as a flonum.  */
  const TFtype b = (a / (((UDItype) 1) << (sizeof (SItype) * 8)));
  /* Convert that to fixed (but not to DItype!),
     and shift it into the high word.  */
  UDItype v = (USItype) b;
  v <<= (sizeof (SItype) * 8);
  /* Remove high part from the TFtype, leaving the low part as flonum.  */
  a -= (TFtype) v;
  /* Convert that to fixed (but not to DItype!) and add it in.
     Sometimes A comes out negative.  This is significant, since
     A has more bits than a long int does.  */
  if (a < 0)
    v -= (USItype) (-a);
  else
    v += (USItype) a;
  return v;
}

#endif
