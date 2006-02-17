/* Public domain.  */
#if __LDBL_MANT_DIG__ == 106 || __LDBL_MANT_DIG__ == 113
typedef int DItype __attribute__ ((mode (DI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef float DFtype __attribute__ ((mode (DF)));
typedef float TFtype __attribute__ ((mode (TF)));

TFtype __floatditf (UDItype);

TFtype
__floatditf (UDItype u)
{
  DFtype dh, dl;

  dh = (SItype) (u >> (sizeof (SItype) * 8));
  dh *= 2.0 * (((UDItype) 1) << ((sizeof (SItype) * 8) - 1));
  dl = (USItype) (u & ((((UDItype) 1) << (sizeof (SItype) * 8)) - 1));

  return (TFtype) dh + (TFtype) dl;
}

#endif

