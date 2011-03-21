/* { dg-do compile } */

/* Test that using an invalid type in a method declaration produces a
   friendly error without a compiler crash.  */

@interface MyClass
@end

@implementation MyClass
- (x) method /* { dg-error "unknown type name" } */
{
  return 0;
}
- (id) method2: (x)argument /* { dg-error "unknown type name" } */
{
  return 0;
}
@end
