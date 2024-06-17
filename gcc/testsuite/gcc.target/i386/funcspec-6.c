/* Test whether all of the 64-bit function specific options are accepted
   without error.  */
/* { dg-do compile { target { ! ia32 } } } */

#include "funcspec-56.inc"

extern void test_uintr (void)			__attribute__((__target__("uintr")));
extern void test_no_uintr (void)		__attribute__((__target__("no-uintr")));
extern void test_arch_foo (void)		__attribute__((__target__("arch=foo"))); /* { dg-error "bad value" } */

extern void test_tune_foo (void)		__attribute__((__target__("tune=foo"))); /* { dg-error "bad value" } */
