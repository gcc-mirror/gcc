/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in ctype transformations are done correctly by
   the compiler.

   Written by Kaveh Ghazi, 2004-04-05.  */

/* { dg-do link } */

extern void link_failure_var(void);

void test(int i)
{
  /* All of these ctype calls should compile-time evaluate to true.  */
#define TEST_CTYPE_CST_TRUE(FN, VALUE) \
  extern void link_failure_##FN##_cst_true(void); \
  extern int FN(int); \
  if (! FN(VALUE)) \
    link_failure_##FN##_cst_true()

  /* All of these ctype calls should compile-time evaluate to false.  */
#define TEST_CTYPE_CST_FALSE(FN, VALUE) \
  extern void link_failure_##FN##_cst_false(void); \
  extern int FN(int); \
  if (FN(VALUE)) \
    link_failure_##FN##_cst_false()
  
  /* All of these ctype calls should compile-time evaluate to true.  */
#define TEST_TOCTYPE_CST_TRUE(FN, VALUE) \
  extern void link_failure_##FN##_cst_true(void); \
  extern int FN(int); \
  if (FN(VALUE) != (VALUE)) \
    link_failure_##FN##_cst_true()

  /* All of these ctype calls should compile-time evaluate to false.  */
#define TEST_TOCTYPE_CST_FALSE(FN, VALUE) \
  extern void link_failure_##FN##_cst_false(void); \
  extern int FN(int); \
  if (FN(VALUE) == (VALUE)) \
    link_failure_##FN##_cst_false()
  
#ifdef __OPTIMIZE__
  TEST_CTYPE_CST_TRUE (isascii, 0);
  TEST_CTYPE_CST_TRUE (isascii, 1);
  TEST_CTYPE_CST_TRUE (isascii, 126);
  TEST_CTYPE_CST_TRUE (isascii, 127);

  TEST_CTYPE_CST_FALSE (isascii, -1);
  TEST_CTYPE_CST_FALSE (isascii, 128);
  TEST_CTYPE_CST_FALSE (isascii, 129);
  TEST_CTYPE_CST_FALSE (isascii, 255);
  TEST_CTYPE_CST_FALSE (isascii, 256);
  TEST_CTYPE_CST_FALSE (isascii, 257);
  TEST_CTYPE_CST_FALSE (isascii, 10000);
  TEST_CTYPE_CST_FALSE (isascii, __INT_MAX__);
  
  /* This ctype call should transform into another expression.  */
  if (isascii(i) != ((i & ~0x7f) == 0))
    link_failure_var();

  TEST_TOCTYPE_CST_TRUE (toascii, 0);
  TEST_TOCTYPE_CST_TRUE (toascii, 1);
  TEST_TOCTYPE_CST_TRUE (toascii, 126);
  TEST_TOCTYPE_CST_TRUE (toascii, 127);

  TEST_TOCTYPE_CST_FALSE (toascii, -1);
  TEST_TOCTYPE_CST_FALSE (toascii, 128);
  TEST_TOCTYPE_CST_FALSE (toascii, 129);
  TEST_TOCTYPE_CST_FALSE (toascii, 255);
  TEST_TOCTYPE_CST_FALSE (toascii, 256);
  TEST_TOCTYPE_CST_FALSE (toascii, 10000);
  TEST_TOCTYPE_CST_FALSE (toascii, __INT_MAX__);

  /* This ctype call should transform into another expression.  */
  if (toascii(i) != (i & 0x7f))
    link_failure_var();

  TEST_CTYPE_CST_TRUE (isdigit, '0');
  TEST_CTYPE_CST_TRUE (isdigit, '1');
  TEST_CTYPE_CST_TRUE (isdigit, '2');
  TEST_CTYPE_CST_TRUE (isdigit, '3');
  TEST_CTYPE_CST_TRUE (isdigit, '4');
  TEST_CTYPE_CST_TRUE (isdigit, '5');
  TEST_CTYPE_CST_TRUE (isdigit, '6');
  TEST_CTYPE_CST_TRUE (isdigit, '7');
  TEST_CTYPE_CST_TRUE (isdigit, '8');
  TEST_CTYPE_CST_TRUE (isdigit, '9');

  TEST_CTYPE_CST_FALSE (isdigit, '0'-1);
  TEST_CTYPE_CST_FALSE (isdigit, '9'+1);
  TEST_CTYPE_CST_FALSE (isdigit, -1);
  TEST_CTYPE_CST_FALSE (isdigit, 0);
  TEST_CTYPE_CST_FALSE (isdigit, 255);
  TEST_CTYPE_CST_FALSE (isdigit, 256);
  TEST_CTYPE_CST_FALSE (isdigit, 10000);
  TEST_CTYPE_CST_FALSE (isdigit, __INT_MAX__);
  
  /* This ctype call should transform into another expression.  */
  if (isdigit(i) != ((unsigned)i - '0' <= 9))
    link_failure_var();
#endif /* __OPTIMIZE__ */
}

int main (void)
{
  return 0;
}
