/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in ctype function attributes are correctly set by
   the compiler.

   Written by Kaveh Ghazi, 2004-03-23.  */

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
  TEST_CTYPE(isalnum);
  TEST_CTYPE(isalpha);
  TEST_CTYPE(isascii);
  TEST_CTYPE(isblank);
  TEST_CTYPE(iscntrl);
  TEST_CTYPE(isdigit);
  TEST_CTYPE(isgraph);
  TEST_CTYPE(islower);
  TEST_CTYPE(isprint);
  TEST_CTYPE(ispunct);
  TEST_CTYPE(isspace);
  TEST_CTYPE(isupper);
  TEST_CTYPE(isxdigit);
  TEST_CTYPE(toascii);
  TEST_CTYPE(tolower);
  TEST_CTYPE(toupper);
#endif /* __OPTIMIZE__ */
}

int main (void)
{
  return 0;
}
