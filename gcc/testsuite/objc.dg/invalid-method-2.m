/* { dg-do compile } */

/* Test that using an invalid type in a method declaration produces a
   friendly error without a compiler crash.  */

#if defined(__has_attribute) && __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
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
