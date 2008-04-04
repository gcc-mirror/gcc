/* { dg-options "-std=gnu99" } */

/* C99 6.5.2.2 Function calls.
   Test scalar passing and return values involving decimal floating
   point types and dereferenced pointers.  */

extern void abort (void);
static int failcnt;

/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

/* A handful of functions that return their Nth _Decimal32
   argument.  */

_Decimal32 __attribute__((noinline))
arg0_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg0;
}

_Decimal32 __attribute__((noinline))
arg1_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg1;
}
	
_Decimal32 __attribute__((noinline))
arg2_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg2;
}
	
_Decimal32 __attribute__((noinline))
arg3_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg3;
}
	
_Decimal32 __attribute__((noinline))
arg4_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg4;
}
	
_Decimal32 __attribute__((noinline))
arg5_32 (_Decimal32 arg0, _Decimal32 arg1, _Decimal32 arg2,
	 _Decimal32 arg3, _Decimal32 arg4, _Decimal32 arg5)
{
  return arg5;
}
	

/* A handful of functions that return their Nth _Decimal64
   argument.  */

_Decimal64 __attribute__((noinline))
arg0_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg0;
}
	
_Decimal64 __attribute__((noinline))
arg1_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg1;
}
	
_Decimal64 __attribute__((noinline))
arg2_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg2;
}
	
_Decimal64 __attribute__((noinline))
arg3_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg3;
}
	
_Decimal64 __attribute__((noinline))
arg4_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg4;
}
	
_Decimal64 __attribute__((noinline))
arg5_64 (_Decimal64 arg0, _Decimal64 arg1, _Decimal64 arg2,
	 _Decimal64 arg3, _Decimal64 arg4, _Decimal64 arg5)
{
  return arg5;
}
	

/* A handful of functions that return their Nth _Decimal128
   argument.  */

_Decimal128 __attribute__((noinline))
arg0_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg0;
}
	
_Decimal128 __attribute__((noinline))
arg1_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg1;
}
	
_Decimal128 __attribute__((noinline))
arg2_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg2;
}
	
_Decimal128 __attribute__((noinline))
arg3_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg3;
}
	
_Decimal128 __attribute__((noinline))
arg4_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg4;
}
	
_Decimal128 __attribute__((noinline))
arg5_128 (_Decimal128 arg0, _Decimal128 arg1, _Decimal128 arg2,
	 _Decimal128 arg3, _Decimal128 arg4, _Decimal128 arg5)
{
  return arg5;
}



_Decimal32 df0 = 0.0df, df1 = 1.0df, df2 = 2.0df,
	   df3 = 3.0df, df4 = 4.0df, df5 = 5.0df;
_Decimal32 *pdf0 = &df0, *pdf1 = &df1, *pdf2 = &df2,
	   *pdf3 = &df3, *pdf4 = &df4, *pdf5 = &df5;
_Decimal64 dd0 = 0.0dd, dd1 = 1.0dd, dd2 = 2.0dd,
	   dd3 = 3.0dd, dd4 = 4.0dd, dd5 = 5.0dd;
_Decimal64 *pdd0 = &dd0, *pdd1 = &dd1, *pdd2 = &dd2,
	   *pdd3 = &dd3, *pdd4 = &dd4, *pdd5 = &dd5;
_Decimal128 dl0 = 0.0dl, dl1 = 1.0dl, dl2 = 2.0dl,
	    dl3 = 3.0dl, dl4 = 4.0dl, dl5 = 5.0dl;
_Decimal128 *pdl0 = &dl0, *pdl1 = &dl1, *pdl2 = &dl2,
	    *pdl3 = &dl3, *pdl4 = &dl4, *pdl5 = &dl5;

int
main ()
{
  /* _Decimal32 variants.  */
  if (arg0_32 (*pdf0, *pdf1, *pdf2, *pdf3, *pdf4, *pdf5) != 0.0df)
    FAILURE
  if (arg1_32 (*pdf0, *pdf1, *pdf2, *pdf3, *pdf4, *pdf5) != 1.0df)
    FAILURE
  if (arg2_32 (*pdf0, *pdf1, *pdf2, *pdf3, *pdf4, *pdf5) != 2.0df)
    FAILURE
  if (arg3_32 (*pdf0, *pdf1, *pdf2, *pdf3, *pdf4, *pdf5) != 3.0df)
    FAILURE
  if (arg4_32 (*pdf0, *pdf1, *pdf2, *pdf3, *pdf4, *pdf5) != 4.0df)
    FAILURE
  if (arg5_32 (*pdf0, *pdf1, *pdf2, *pdf3, *pdf4, *pdf5) != 5.0df)
    FAILURE

  /* _Decimal64 variants.  */
  if (arg0_64 (*pdd0, *pdd1, *pdd2, *pdd3, *pdd4, *pdd5) != 0.0dd)
    FAILURE
  if (arg1_64 (*pdd0, *pdd1, *pdd2, *pdd3, *pdd4, *pdd5) != 1.0dd)
    FAILURE
  if (arg2_64 (*pdd0, *pdd1, *pdd2, *pdd3, *pdd4, *pdd5) != 2.0dd)
    FAILURE
  if (arg3_64 (*pdd0, *pdd1, *pdd2, *pdd3, *pdd4, *pdd5) != 3.0dd)
    FAILURE
  if (arg4_64 (*pdd0, *pdd1, *pdd2, *pdd3, *pdd4, *pdd5) != 4.0dd)
    FAILURE
  if (arg5_64 (*pdd0, *pdd1, *pdd2, *pdd3, *pdd4, *pdd5) != 5.0dd)
    FAILURE

  /* _Decimal128 variants.  */
  if (arg0_128 (*pdl0, *pdl1, *pdl2, *pdl3, *pdl4, *pdl5) != 0.0dl)
    FAILURE
  if (arg1_128 (*pdl0, *pdl1, *pdl2, *pdl3, *pdl4, *pdl5) != 1.0dl)
    FAILURE
  if (arg2_128 (*pdl0, *pdl1, *pdl2, *pdl3, *pdl4, *pdl5) != 2.0dl)
    FAILURE
  if (arg3_128 (*pdl0, *pdl1, *pdl2, *pdl3, *pdl4, *pdl5) != 3.0dl)
    FAILURE
  if (arg4_128 (*pdl0, *pdl1, *pdl2, *pdl3, *pdl4, *pdl5) != 4.0dl)
    FAILURE
  if (arg5_128 (*pdl0, *pdl1, *pdl2, *pdl3, *pdl4, *pdl5) != 5.0dl)
    FAILURE

  if (failcnt != 0)
    abort ();

  return 0;
}
