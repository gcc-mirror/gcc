#ifndef DEJAGNU_GTEST_H
#define DEJAGNU_GTEST_H 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef __cplusplus
#include <string>
#endif

struct dejagnu_gtest_test
{
  const char *name;
  void (*fn) (void);
  struct dejagnu_gtest_test *next;  
};
struct dejagnu_gtest_test *dejagnu_gtest_test_first, *dejagnu_gtest_test_last;
int dejagnu_gtest_test_death_num, dejagnu_gtest_test_death_cur_num;

#define TEST(cond, name) \
static void cond##_##name##_fn (void);				\
static struct dejagnu_gtest_test cond##_##name##_struct		\
  = { #cond "_" #name, cond##_##name##_fn, NULL };		\
static __attribute__((__constructor__)) void			\
cond##_##name##_ctor (void)					\
{								\
  if (strncmp (#name, "DISABLED_", 9) == 0)			\
    return;							\
  if (dejagnu_gtest_test_first == NULL)				\
    dejagnu_gtest_test_first = &cond##_##name##_struct;		\
  else								\
    dejagnu_gtest_test_last->next = &cond##_##name##_struct;	\
  dejagnu_gtest_test_last = &cond##_##name##_struct;		\
}								\
static void							\
cond##_##name##_fn (void)

#ifndef __cplusplus
# define DEJAGNU_GTEST_TOCSTR(regex) (regex)
#else
static inline const char *DEJAGNU_GTEST_TOCSTR(const char *x) { return x; }
static inline const char *DEJAGNU_GTEST_TOCSTR(const std::string &x) { return x.c_str (); }
#endif

#define EXPECT_DEATH(statement, regex) \
do								\
  {								\
    ++dejagnu_gtest_test_death_cur_num;				\
    if (dejagnu_gtest_test_death_num == 0)			\
      {								\
	fprintf (stderr, "DEJAGNU_GTEST_EXPECT_DEATH%d %s "	\
			 "DEJAGNU_GTEST_EXPECT_DEATH%d %s "	\
			 "DEJAGNU_GTEST_EXPECT_DEATH%d\n",	\
		 dejagnu_gtest_test_death_cur_num, #statement,	\
		 dejagnu_gtest_test_death_cur_num,		\
		 DEJAGNU_GTEST_TOCSTR (regex),			\
		 dejagnu_gtest_test_death_cur_num);		\
      }								\
    else if (dejagnu_gtest_test_death_cur_num			\
	     == dejagnu_gtest_test_death_num)			\
      {								\
	statement;						\
      }								\
  }								\
while (0)

#define EXPECT_TRUE(condition) \
  if (!(condition))						\
    {								\
      fprintf (stderr, "%s",					\
	       "EXPECT_TRUE failed: " #condition "\n");		\
      exit (1);							\
    }
#define EXPECT_FALSE(condition) EXPECT_TRUE (!condition)
#define EXPECT_EQ(expected, actual) EXPECT_TRUE ((expected) == (actual))
#define EXPECT_NE(expected, actual) EXPECT_TRUE ((expected) != (actual))
#define EXPECT_LT(expected, actual) EXPECT_TRUE ((expected) < (actual))
#define EXPECT_LE(expected, actual) EXPECT_TRUE ((expected) <= (actual))
#define EXPECT_GT(expected, actual) EXPECT_TRUE ((expected) > (actual))
#define EXPECT_GE(expected, actual) EXPECT_TRUE ((expected) >= (actual))
#define ASSERT_DEATH(statement, regex) EXPECT_DEATH (statement, regex)
#define ASSERT_TRUE(condition) EXPECT_TRUE (condition)
#define ASSERT_FALSE(condition) EXPECT_FALSE (condition)
#define ASSERT_EQ(expected, actual) EXPECT_EQ (expected, actual)
#define ASSERT_NE(expected, actual) EXPECT_NE (expected, actual)
#define ASSERT_LT(expected, actual) EXPECT_LT (expected, actual)
#define ASSERT_LE(expected, actual) EXPECT_LE (expected, actual)
#define ASSERT_GT(expected, actual) EXPECT_GT (expected, actual)
#define ASSERT_GE(expected, actual) EXPECT_GE (expected, actual)

int
main (int argc, const char **argv)
{
  const char *test = NULL;
  struct dejagnu_gtest_test *t;
  if (argc > 1)
    test = argv[1];
  else
    test = getenv ("DEJAGNU_GTEST_ARG");
  if (test == NULL)
    for (t = dejagnu_gtest_test_first; t; t = t->next)
      fprintf (stderr, "DEJAGNU_GTEST_TEST %s\n", t->name);
  else
    {
      const char *p = strchr (test, ':');
      if (p != NULL)
	dejagnu_gtest_test_death_num = atoi (p + 1);
      for (t = dejagnu_gtest_test_first; t; t = t->next)
	if (p != NULL
	    ? (strncmp (test, t->name, p - test) == 0
	       && t->name[p - test] == '\0')
	    : (strcmp (test, t->name) == 0))
	  break;
      EXPECT_TRUE (t != NULL);
      t->fn ();
    }
  return 0;
}

#endif
