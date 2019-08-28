/* PR tree-optimization/71625 - conversion of braced initializers to strings
   Verify that array elements have the expected values regardless of sign
   and non-ASCII execution character set.
   { dg-do compile }
   { dg-require-iconv "IBM1047" }
   { dg-options "-O -Wall -fexec-charset=IBM1047 -fdump-tree-gimple -fdump-tree-optimized" } */

#include "strlenopt.h"

const char a[] = { 'a', 129, 0 };
const signed char b[] = { 'b', 130, 0 };
const unsigned char c[] = { 'c', 131, 0 };

const char s[] = "a\201";
const signed char ss[] = "b\202";
const unsigned char us[] = "c\203";


#define A(expr)   ((expr) ? (void)0 : __builtin_abort ())

void test_values (void)
{
  A (a[0] == a[1]);
  A (a[1] == 'a');

  A (b[0] == b[1]);
  A (b[1] == (signed char)'b');

  A (c[0] == c[1]);
  A (c[1] == (unsigned char)'c');
}

void test_lengths (void)
{
  A (2 == strlen (a));
  A (2 == strlen ((const char*)b));
  A (2 == strlen ((const char*)c));
}

void test_contents (void)
{
  A (0 == strcmp (a, s));
  A (0 == strcmp ((const char*)b, (const char*)ss));
  A (0 == strcmp ((const char*)c, (const char*)us));
}


/* { dg-final { scan-tree-dump-times "strlen1" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "strcmp" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "abort" 0 "optimized" } } */
