/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in wctype function attributes are correctly set
   by the compiler.

   Written by Kaveh Ghazi, 2004-03-25.  */

/* { dg-do link } */


void test(int i)
{
  /* All of these ctype functions should be const/pure and thus
     eliminated.  */
#define TEST_CTYPE(FN) \
  extern int FN(int); \
  extern void link_failure_##FN(void); \
  if (FN(i) != FN(i)) \
    link_failure_##FN()
  
#ifdef __OPTIMIZE__
  TEST_CTYPE(iswalnum);
  TEST_CTYPE(iswalpha);
  TEST_CTYPE(iswblank);
  TEST_CTYPE(iswcntrl);
  TEST_CTYPE(iswdigit);
  TEST_CTYPE(iswgraph);
  TEST_CTYPE(iswlower);
  TEST_CTYPE(iswprint);
  TEST_CTYPE(iswpunct);
  TEST_CTYPE(iswspace);
  TEST_CTYPE(iswupper);
  TEST_CTYPE(iswxdigit);
  TEST_CTYPE(towlower);
  TEST_CTYPE(towupper);
#endif /* __OPTIMIZE__ */
}

int main (void)
{
  return 0;
}
