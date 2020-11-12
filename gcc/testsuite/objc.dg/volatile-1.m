/* Test for proper handling of volatile parameters in ObjC methods.  */
/* Contributed by Ziemowit Laski  <zlaski@apple.com>  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface Test
-(void) test2: (volatile int) a;
@end

@implementation Test
-(void) test2: (volatile int) a
{
  /* The following assignment should NOT be optimized away.  */
  a = 1;
}
@end

/* { dg-final { scan-assembler "li r\[0-9\]+,1" { target powerpc*-*-darwin* } } } */
