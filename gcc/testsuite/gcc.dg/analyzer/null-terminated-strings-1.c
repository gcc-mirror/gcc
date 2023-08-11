#include "analyzer-decls.h"

#define NULL ((void *)0)
typedef __SIZE_TYPE__ size_t;

void test_terminated (void)
{
  __analyzer_get_strlen ("abc"); /* { dg-bogus "" } */
}

void test_unterminated (void)
{
  char buf[3] = "abc";
  __analyzer_get_strlen (buf); /* { dg-warning "passing pointer to unterminated string '&buf' as argument 1 of '__analyzer_get_strlen'" } */
}

void test_embedded_nul (void)
{
  char buf[3] = "a\0c";
  __analyzer_get_strlen (buf); /* { dg-bogus "" } */
}

void test_fully_initialized_but_unterminated (void)
{
  char buf[3];
  buf[0] = 'a';
  buf[1] = 'b';
  buf[2] = 'c';
  __analyzer_get_strlen (buf); /* { dg-warning "passing pointer to unterminated string '&buf' as argument 1 of '__analyzer_get_strlen'" "" { xfail *-*-* } } */
}
