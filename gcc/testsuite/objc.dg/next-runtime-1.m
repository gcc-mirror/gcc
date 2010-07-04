/* Test that the correct version number (6) is set in the module descriptor
   when compiling for the NeXT runtime.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"

@interface FooBar: Object
- (void)boo;
@end

@implementation FooBar
- (void)boo { }
@end

/* { dg-final { scan-assembler "L_OBJC_MODULES:\n\[ \t\]*\.long\t6\n" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler "L_OBJC_MODULES:\n\[ \t\]*\.quad\t6\n" { target { *-*-darwin* && {  lp64 } } } } } */
