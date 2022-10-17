/* { dg-do compile { target x86_64-*-* } } */

#include "analyzer-decls.h"

int test_out (void)
{
  int dst_a, dst_b;
  asm ("mov 42, %0"
       : "=r" (dst_a));
  asm ("mov 42, %0"
       : "=r" (dst_b));
  __analyzer_eval (dst_a == dst_b); /* { dg-warning "TRUE" } */
  return dst_a;
}

int test_out_in (int src_a)
{
  int dst_a, dst_b;
  asm ("mov %1, %0"
       : "=r" (dst_a)
       : "r" (src_a));
  asm ("mov %1, %0"
       : "=r" (dst_b)
       : "r" (src_a));
  __analyzer_eval (dst_a == dst_b); /* { dg-warning "TRUE" } */
  return dst_a;
}

int test_out_in_in (int src_a, int src_b)
{
  int dst_a, dst_b;
  asm ("mov %1, %0;\n"
       "add %2, %0"
       : "=r" (dst_a)
       : "r" (src_a),
	 "r" (src_b));
  asm ("mov %1, %0;\n"
       "add %2, %0"
       : "=r" (dst_b)
       : "r" (src_a),
	 "r" (src_b));
  __analyzer_eval (dst_a == dst_b); /* { dg-warning "TRUE" } */
  return dst_a;
}

void test_inout_1 (int v)
{
  int saved = v;
  int result_a, result_b;
  asm ("dec %0"
       : "+r" (v));
  result_a = v;

  asm ("dec %0"
       : "+r" (v));
  result_b = v;

  __analyzer_eval (v == saved); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (v == result_a); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (v == result_b); /* { dg-warning "TRUE" } */
}

void test_inout_2 (void)
{
  int v;
  int result_a, result_b;
  asm ("dec %0" /* { dg-warning "use of uninitialized value 'v'" } */
       : "+r" (v));
}
