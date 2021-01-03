/* Test for graceful handling of anonymous ivars.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface Foo {
   unsigned char : 1;
   int e: 3;
   signed: 2;
   float f;
}
@end

@implementation Foo
@end

