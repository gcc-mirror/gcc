/* { dg-do run } */
/* { dg-options "-msim" } */
/* Note: The -msim above is actually there to override the default
   options which include -ansi -pendantic and -Wlong-long...   */

extern int printf (const char *, ...);
extern void exit (int);
extern void abort (void);

extern signed long        _COM_CONVf32s (float);
extern unsigned long      _COM_CONVf32u (float);
extern float              _COM_CONV32sf (signed long);
extern float              _COM_CONV32uf (unsigned long);
extern float              _COM_ADDf (float, float);
extern float              _COM_SUBf (float, float);
extern float              _COM_MULf (float, float);
extern float              _COM_DIVf (float, float);
extern int                _COM_CMPLTf (float, float);

extern long long          _COM_MUL64 (long long, long long);
extern signed long long   _COM_DIV64s (long long, long long);
extern unsigned long long _COM_DIV64u (unsigned long long, unsigned long long);
extern long long          _COM_SHLL64 (long long, int);
extern long long          _COM_SHLR64 (long long, int);
extern long long          _COM_SHAR64 (long long, int);
extern signed long long   _COM_CONVf64s (float);
extern unsigned long long _COM_CONVf64u (float);
extern signed long long   _COM_CONVd64s (double);
extern unsigned long long _COM_CONVd64u (double);
extern float              _COM_CONV64sf (signed long long);
extern float              _COM_CONV64uf (unsigned long long);
extern double             _COM_CONV64sd (signed long long);
extern double             _COM_CONV64ud (unsigned long long);
extern signed long long   _COM_MOD64s (long long, long long);
extern unsigned long long _COM_MOD64u (unsigned long long, unsigned long long);
extern int                _COM_CMPLT64s (long long, long long);
extern int                _COM_CMPLT64u (unsigned long long, unsigned long long);
extern int                _COM_CMPGT64s (long long, long long);
extern int                _COM_CMPGT64u (unsigned long long, unsigned long long);
extern int                _COM_CMPLE64s (long long, long long);
extern int                _COM_CMPLE64u (unsigned long long, unsigned long long);
extern int                _COM_CMPGE64s (long long, long long);
extern int                _COM_CMPGE64u (unsigned long long, unsigned long long);
extern int                _COM_CMPEQ64 (long long, long long);
extern int                _COM_CMPNE64 (long long, long long);

extern double             _COM_ADDd (double, double);
extern double             _COM_SUBd (double, double);
extern double             _COM_MULd (double, double);
extern double             _COM_DIVd (double, double);
extern signed long        _COM_CONVd32s (double);
extern unsigned long      _COM_CONVd32u (double);
extern double             _COM_CONV32sd (signed long);
extern double             _COM_CONV32ud (unsigned long);
extern double             _COM_CONVfd (float);
extern float              _COM_CONVdf (double);
extern double             _COM_NEGd (double);


/* #define DEBUG 1 */

#ifdef DEBUG
# define TEST1(func,arg1,result)	if (func (arg1) != result) printf ("fail: " #func " (" #arg1 ") returns %x rather than " #result "\n", func (arg1))
# define TEST2(func,arg1,arg2,result)	if (func (arg1, arg2) != result) printf ("fail: " #func " (" #arg1 ", " #arg2 ") returns %x rather than " #result "\n", func (arg1, arg2))
# define TEST_CMP(func, low_arg, high_arg, lt_result, eq_result, gt_result)	\
  do									\
    {									\
      int res;								\
									\
      if ((res = func (low_arg, high_arg)) != lt_result)   printf ("fail: " #func " (" #low_arg ", " #high_arg ") returns %d rather than %d\n", res, lt_result); \
      if ((res = func (high_arg, low_arg)) != gt_result)   printf ("fail: " #func " (" #high_arg ", " #low_arg ") returns %d rather than %d\n", res, gt_result); \
      if ((res = func (low_arg, low_arg))  != eq_result)   printf ("fail: " #func " (" #low_arg ", " #low_arg ") returns %d rather than %d\n", res, eq_result); \
    } \
  while (0)
#else
# define TEST1(func,arg1,result)	if (func (arg1) != result) abort ()
# define TEST2(func,arg1,arg2,result)	if (func (arg1, arg2) != result) abort ()
# define TEST_CMP(func,low,high,lt_res,eq_res,gt_res)	\
  if (   (func (low, high) != lt_res)			\
      || (func (high, low) != gt_res)			\
      || (func (low, low)  != eq_res))			\
    abort ();
#endif


int
main (void)
{
#ifdef DEBUG
  printf ("Tests starting\n");
#endif

  TEST1 (_COM_CONVf32s, -2.0f, -2);
  TEST1 (_COM_CONVf32u, -2.0f, (unsigned) -2);
  TEST1 (_COM_CONV32sf, -2, -2.0f);
  TEST1 (_COM_CONV32uf, 2, 2.0f);
  TEST2 (_COM_ADDf, 1.0f, 2.0f, 3.0f);
  TEST2 (_COM_SUBf, 3.0f, 2.0f, 1.0f);
  TEST2 (_COM_MULf, 2.0f, 3.0f, 6.0f);
  TEST2 (_COM_DIVf, 6.0f, 2.0f, 3.0f);
  TEST_CMP (_COM_CMPLTf, 1.0f, 2.0f, 1, 0, 0);
  TEST_CMP (_COM_CMPGTf, 1.0f, 2.0f, 0, 0, 1);
  TEST_CMP (_COM_CMPLEf, 1.0f, 2.0f, 1, 1, 0);
  TEST_CMP (_COM_CMPGEf, 1.0f, 2.0f, 0, 1, 1);
  TEST_CMP (_COM_CMPEQf, 1.0f, 2.0f, 0, 1, 0);
  TEST_CMP (_COM_CMPNEf, 1.0f, 2.0f, 1, 0, 1);


  TEST2 (_COM_MUL64, 2LL, 4LL, 8LL);
  TEST2 (_COM_DIV64s, 6LL, 3LL, 2LL);
  TEST2 (_COM_DIV64u, 6ULL, 3ULL, 2ULL);
  TEST2 (_COM_SHLL64, 6LL, 3, 48LL);
  TEST2 (_COM_SHLR64, 8LL, 2, 2LL);
  TEST2 (_COM_SHAR64, -1LL, 2, -1LL);  
  TEST1 (_COM_CONVf64s, -2.0f, -2LL);
  TEST1 (_COM_CONVf64u, 2.0f, 2ULL);
  TEST1 (_COM_CONVd64s, -2.0, -2LL);
  TEST1 (_COM_CONVd64u, 2.0, 2ULL);
  TEST1 (_COM_CONV64sf, -2LL, -2.0f);
  TEST1 (_COM_CONV64uf, 2ULL, 2.0f);
  TEST1 (_COM_CONV64sd, -2LL, -2.0);
  TEST1 (_COM_CONV64ud, 2ULL, 2.0);
  TEST2 (_COM_MOD64s, 4LL, 3LL, 1LL);
  TEST2 (_COM_MOD64u, 4ULL, 3ULL, 1ULL);
  TEST_CMP (_COM_CMPLT64s, 1LL, 2LL, 1, 0, 0);
  TEST_CMP (_COM_CMPLT64u, 1ULL, 2ULL, 1, 0, 0);
  TEST_CMP (_COM_CMPGT64s, 1LL, 2LL, 0, 0, 1);
  TEST_CMP (_COM_CMPGT64u, 1ULL, 2ULL, 0, 0, 1);
  TEST_CMP (_COM_CMPLE64s, 1LL, 2LL, 1, 1, 0);
  TEST_CMP (_COM_CMPLE64u, 1ULL, 2ULL, 1, 1, 0);
  TEST_CMP (_COM_CMPGE64s, 1LL, 2LL, 0, 1, 1);
  TEST_CMP (_COM_CMPGE64u, 1ULL, 2ULL, 0, 1, 1);
  TEST_CMP (_COM_CMPEQ64, 1LL, 2LL, 0, 1, 0);
  TEST_CMP (_COM_CMPNE64, 1LL, 2LL, 1, 0, 1);


  TEST2 (_COM_ADDd, 1.0, 2.0, 3.0);
  TEST2 (_COM_SUBd, 3.0, 2.0, 1.0);
  TEST2 (_COM_MULd, 2.0, 3.0, 6.0);
  TEST2 (_COM_DIVd, 6.0, 2.0, 3.0);
  TEST1 (_COM_CONVd32s, -2.0, -2);
  TEST1 (_COM_CONVd32u, -2.0, (unsigned) -2);
  TEST1 (_COM_CONV32sd, -2, -2.0);
  TEST1 (_COM_CONV32ud, 2, 2.0);
  TEST1 (_COM_CONVfd, 2.0f, 2.0);
  TEST1 (_COM_CONVdf, 2.0, 2.0f);
  TEST1 (_COM_NEGd, -2.0, 2.0);
  TEST_CMP (_COM_CMPLTd, 1.0, 2.0, 1, 0, 0);
  TEST_CMP (_COM_CMPGTd, 1.0, 2.0, 0, 0, 1);
  TEST_CMP (_COM_CMPLEd, 1.0, 2.0, 1, 1, 0);
  TEST_CMP (_COM_CMPGEd, 1.0, 2.0, 0, 1, 1);
  TEST_CMP (_COM_CMPEQd, 1.0, 2.0, 0, 1, 0);
  TEST_CMP (_COM_CMPNEd, 1.0, 2.0, 1, 0, 1);
  
#ifdef DEBUG
  printf ("Tests finished\n");
#endif
  exit (0);
}
