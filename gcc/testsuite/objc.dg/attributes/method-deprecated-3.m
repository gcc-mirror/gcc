/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

/* Test that __attribute__ ((__deprecated__)) works as well as __attribute__ ((deprecated)).  */
@interface MyClass
{
  Class isa;
} 
+ (int) deprecatedClassMethod: (id)firstObject, ...    __attribute__((__deprecated__));
- (int) deprecatedInstanceMethod: (id)firstobject, ... __attribute__((__deprecated__));
@end

void foo (void)
{
  MyClass *object = nil;

  [MyClass deprecatedClassMethod: object, nil];           /* { dg-warning "is deprecated" } */
  [object deprecatedInstanceMethod: object, nil];         /* { dg-warning "is deprecated" } */ 
}
