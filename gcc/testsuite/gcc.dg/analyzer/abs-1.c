#include "analyzer-decls.h"

extern long int labs (long int x)
  __attribute__ ((__nothrow__ , __leaf__))
  __attribute__ ((__const__));

long int test_1 (long int x)
{
  return labs (x);
}

static long __attribute__((noinline))
hide_long (long x)
{
  return x;
}
  
long int test_2 (long int x)
{
  __analyzer_eval (labs (hide_long (42)) == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (labs (hide_long (-17)) == 17); /* { dg-warning "TRUE" } */
}
