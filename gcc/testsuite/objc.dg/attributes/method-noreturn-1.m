/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>
#include <stdlib.h>

@interface MyClass
{
  Class isa;
} 
+ (id) method1 __attribute__ ((noreturn));
- (id) method2 __attribute__ ((noreturn));
+ (id) method3 __attribute__ ((noreturn));
- (id) method4 __attribute__ ((noreturn));
@end

@implementation MyClass
+ (id) method1
{
  return self;  /* { dg-warning "function declared .noreturn. has a .return. statement" } */
}               /* { dg-warning ".noreturn. function does return" "" { target *-*-* } 20 } */
- (id) method2
{
  return self;  /* { dg-warning "function declared .noreturn. has a .return. statement" } */
}               /* { dg-warning ".noreturn. function does return" "" { target *-*-* } 24 } */
+ (id) method3
{
  abort ();
}
- (id) method4
{
  abort ();
}
@end
