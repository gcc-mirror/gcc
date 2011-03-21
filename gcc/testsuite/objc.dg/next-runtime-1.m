/* Test that the correct version number (6) is set in the module descriptor
   when compiling for the NeXT runtime ABI=0 - and that the MODULE descriptor
   is not emitted at all for ABI 2.  */
/* modified from a testcase added by: Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-fobjc-abi-version=1" } { "" } } */
/* { dg-options "-fobjc-abi-version=0" { target { *-*-darwin* && { ! lp64 } } } } */

@interface FooBar
- (void)boo;
@end

@implementation FooBar
- (void)boo { }
@end

/* { dg-final { scan-assembler "L_OBJC_Module:\n\[ \t\]*\.long\t6\n" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler-not "L_OBJC_Module" { target { *-*-darwin* && {  lp64 } } } } } */
