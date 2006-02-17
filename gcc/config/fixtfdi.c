/* Public domain.  */
#if __LDBL_MANT_DIG__ == 106 || __LDBL_MANT_DIG__ == 113
typedef int DItype __attribute__ ((mode (DI)));
typedef float TFtype __attribute__ ((mode (TF)));

DItype __fixtfdi (TFtype);
DItype __fixunstfdi (TFtype);


DItype
__fixtfdi (TFtype x)
{
  if (x < 0)
    return - __fixunstfdi (-x);
  return __fixunstfdi (x);
}

#endif
