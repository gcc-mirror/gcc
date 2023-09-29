#include <string.h>

#ifdef __cplusplus
#define CONST_CAST(type) const_cast<type>
#else
#define CONST_CAST(type)
#endif

/* PR analyzer/95007.  */

void test_1 (void)
{
  char *s = CONST_CAST(char *)("foo");
  s[0] = 'g'; /* { dg-warning "write to string literal" } */
}

/* PR c/83347.  */

void test_2 (void)
{
  // Technically irrelevant for C++ as fpermissive will warn about invalid conversion.
  memcpy (CONST_CAST(char *)("abc"), "def", 3); /* { dg-warning "write to string literal" } */
}

/* PR c/83347.  */

static char * __attribute__((noinline))
called_by_test_3 (void)
{
  return (char *)"foo";
}

void test_3 (void)
{
  char *s = called_by_test_3 ();
  s[1] = 'a'; /* { dg-warning "write to string literal" } */
}

static char * __attribute__((noinline))
called_by_test_4 (int flag)
{
  if (flag)
    return (char *)"foo";
  else
    return (char *)"bar";
}

void test_4 (void)
{
  char *s = called_by_test_4 (0);
  s[1] = 'z'; /* { dg-warning "write to string literal" } */
}

static char * __attribute__((noinline))
called_by_test_5 (int flag)
{
  if (flag)
    return (char *)"foo";
  else
    return (char *)"bar";
}

void test_5 (int flag)
{
  char *s = called_by_test_5 (flag);
  s[1] = 'z'; /* We miss this one, unless we disable state merging.  */
}
