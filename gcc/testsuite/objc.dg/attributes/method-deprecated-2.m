/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
} 
+ (int) deprecatedClassMethod: (id)firstObject, ...    __attribute__((sentinel)) __attribute__((deprecated));
- (int) deprecatedInstanceMethod: (id)firstobject, ... __attribute__((sentinel)) __attribute__((deprecated));
@end

/* Test that deprecation warnings are produced even if the method is
   also marked with another attribute too (this is to test the
   processing of multiple attributes).  */
void foo (void)
{
  MyClass *object = nil;

  [MyClass deprecatedClassMethod: object, nil];           /* { dg-warning "is deprecated" } */
  [object deprecatedInstanceMethod: object, nil];         /* { dg-warning "is deprecated" } */ 
}
