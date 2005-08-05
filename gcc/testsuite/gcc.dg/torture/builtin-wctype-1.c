/* Copyright (C) 2004, 2005  Free Software Foundation.

   Verify that built-in wctype function attributes are correctly set
   by the compiler.

   Written by Kaveh Ghazi, 2004-03-25.  */

/* { dg-do link } */

/* Use the target type definitions if we can.  */ 
#ifndef __WINT_TYPE__
#define __WINT_TYPE__ int
#endif

#ifndef __WCHAR_TYPE__
#define __WCHAR_TYPE__ int
#endif

void test(int i)
{
  /* All of these ctype functions should be const/pure and thus
     eliminated.  */
#define TEST_IS_WCTYPE(FN) \
  extern int FN(__WINT_TYPE__); \
  extern void link_failure_##FN(void); \
  if (FN(i) != FN(i)) \
    link_failure_##FN()

#define TEST_TO_WCTYPE(FN) \
  extern __WINT_TYPE__ FN(__WINT_TYPE__); \
  extern void link_failure_##FN(void); \
  if (FN(i) != FN(i)) \
    link_failure_##FN()

  
#ifdef __OPTIMIZE__
  TEST_IS_WCTYPE(iswalnum);
  TEST_IS_WCTYPE(iswalpha);
  TEST_IS_WCTYPE(iswblank);
  TEST_IS_WCTYPE(iswcntrl);
  TEST_IS_WCTYPE(iswdigit);
  TEST_IS_WCTYPE(iswgraph);
  TEST_IS_WCTYPE(iswlower);
  TEST_IS_WCTYPE(iswprint);
  TEST_IS_WCTYPE(iswpunct);
  TEST_IS_WCTYPE(iswspace);
  TEST_IS_WCTYPE(iswupper);
  TEST_IS_WCTYPE(iswxdigit);
  TEST_TO_WCTYPE(towlower);
  TEST_TO_WCTYPE(towupper);
#endif /* __OPTIMIZE__ */
}

int main (void)
{
  return 0;
}
