/* A self-testing framework, for use by -fself-test.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_SELFTEST_H
#define GCC_SELFTEST_H

/* The selftest code should entirely disappear in a production
   configuration, hence we guard all of it with #if CHECKING_P.  */

#if CHECKING_P

namespace selftest {

/* The entrypoint for running all tests.  */

extern void run_tests ();

/* Record the successful outcome of some aspect of the test.  */

extern void pass (const char *file, int line, const char *msg);

/* Report the failed outcome of some aspect of the test and abort.  */

extern void fail (const char *file, int line, const char *msg);

/* Declarations for specific families of tests (by source file), in
   alphabetical order.  */
extern void bitmap_c_tests ();
extern void diagnostic_show_locus_c_tests ();
extern void et_forest_c_tests ();
extern void fold_const_c_tests ();
extern void function_tests_c_tests ();
extern void gimple_c_tests ();
extern void hash_map_tests_c_tests ();
extern void hash_set_tests_c_tests ();
extern void input_c_tests ();
extern void pretty_print_c_tests ();
extern void rtl_tests_c_tests ();
extern void spellcheck_c_tests ();
extern void tree_c_tests ();
extern void tree_cfg_c_tests ();
extern void vec_c_tests ();
extern void wide_int_cc_tests ();

extern int num_passes;

} /* end of namespace selftest.  */

/* Macros for writing tests.  */

/* Evaluate EXPR and coerce to bool, calling
   ::selftest::pass if it is true,
   ::selftest::fail if it false.  */

#define ASSERT_TRUE(EXPR)				\
  SELFTEST_BEGIN_STMT					\
  const char *desc = "ASSERT_TRUE (" #EXPR ")";		\
  bool actual = (EXPR);					\
  if (actual)						\
    ::selftest::pass (__FILE__, __LINE__, desc);	\
  else							\
    ::selftest::fail (__FILE__, __LINE__, desc);		\
  SELFTEST_END_STMT

/* Evaluate EXPR and coerce to bool, calling
   ::selftest::pass if it is false,
   ::selftest::fail if it true.  */

#define ASSERT_FALSE(EXPR)					\
  SELFTEST_BEGIN_STMT						\
  const char *desc = "ASSERT_FALSE (" #EXPR ")";		\
  bool actual = (EXPR);					\
  if (actual)							\
    ::selftest::fail (__FILE__, __LINE__, desc);				\
  else								\
    ::selftest::pass (__FILE__, __LINE__, desc);				\
  SELFTEST_END_STMT

/* Evaluate EXPECTED and ACTUAL and compare them with ==, calling
   ::selftest::pass if they are equal,
   ::selftest::fail if they are non-equal.  */

#define ASSERT_EQ(EXPECTED, ACTUAL)			       \
  SELFTEST_BEGIN_STMT					       \
  const char *desc = "ASSERT_EQ (" #EXPECTED ", " #ACTUAL ")"; \
  if ((EXPECTED) == (ACTUAL))				       \
    ::selftest::pass (__FILE__, __LINE__, desc);			       \
  else							       \
    ::selftest::fail (__FILE__, __LINE__, desc);			       \
  SELFTEST_END_STMT

/* Evaluate EXPECTED and ACTUAL and compare them with !=, calling
   ::selftest::pass if they are non-equal,
   ::selftest::fail if they are equal.  */

#define ASSERT_NE(EXPECTED, ACTUAL)			       \
  SELFTEST_BEGIN_STMT					       \
  const char *desc = "ASSERT_NE (" #EXPECTED ", " #ACTUAL ")"; \
  if ((EXPECTED) != (ACTUAL))				       \
    ::selftest::pass (__FILE__, __LINE__, desc);			       \
  else							       \
    ::selftest::fail (__FILE__, __LINE__, desc);			       \
  SELFTEST_END_STMT

/* Evaluate EXPECTED and ACTUAL and compare them with strcmp, calling
   ::selftest::pass if they are equal,
   ::selftest::fail if they are non-equal.  */

#define ASSERT_STREQ(EXPECTED, ACTUAL)			       \
  SELFTEST_BEGIN_STMT					       \
  const char *desc = "ASSERT_STREQ (" #EXPECTED ", " #ACTUAL ")"; \
  const char *expected_ = (EXPECTED);				  \
  const char *actual_ = (ACTUAL);				  \
  if (0 == strcmp (expected_, actual_))				  \
    ::selftest::pass (__FILE__, __LINE__, desc);			       \
  else							       \
    ::selftest::fail (__FILE__, __LINE__, desc);			       \
  SELFTEST_END_STMT

/* Evaluate PRED1 (VAL1), calling ::selftest::pass if it is true,
   ::selftest::fail if it is false.  */

#define ASSERT_PRED1(PRED1, VAL1)			\
  SELFTEST_BEGIN_STMT					\
  const char *desc = "ASSERT_PRED1 (" #PRED1 ", " #VAL1 ")";	\
  bool actual = (PRED1) (VAL1);				\
  if (actual)						\
    ::selftest::pass (__FILE__, __LINE__, desc);			\
  else							\
    ::selftest::fail (__FILE__, __LINE__, desc);			\
  SELFTEST_END_STMT

#define SELFTEST_BEGIN_STMT do {
#define SELFTEST_END_STMT   } while (0)

#endif /* #if CHECKING_P */

#endif /* GCC_SELFTEST_H */
