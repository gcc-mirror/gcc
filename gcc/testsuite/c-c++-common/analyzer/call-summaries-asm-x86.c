/* { dg-do compile { target x86_64-*-* } } */
/* { dg-additional-options "-fanalyzer-call-summaries --param analyzer-min-snodes-for-call-summary=0" } */

#include "analyzer-decls.h"

int returns_asm_value (void)
{
  int dst;
  asm ("mov 42, %0"
       : "=r" (dst));
  return dst;
}

void test_returns_asm_value (void)
{
  int a, b;
  a = returns_asm_value ();
  b = returns_asm_value ();
  __analyzer_eval (a == b); /* { dg-warning "TRUE" } */
}
