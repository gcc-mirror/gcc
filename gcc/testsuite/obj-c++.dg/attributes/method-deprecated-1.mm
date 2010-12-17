/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, October 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface MyClass
{
  Class isa;
} 
+ (int) method;
- (int) method;
+ (int) deprecatedClassMethod __attribute__((deprecated));
- (int) deprecatedInstanceMethod __attribute__((deprecated));
@end

/* Test that deprecation warnings are produced, but not if the
   receiver is of type 'id'.  */
void foo (void)
{
  Class c;
  id object;
  MyClass *another_object;

  [c method];
  [object method];
  [c deprecatedClassMethod];
  [object deprecatedInstanceMethod];

  [object method];
  [another_object method];
  [MyClass deprecatedClassMethod];           /* { dg-warning "is deprecated" } */
  [another_object deprecatedInstanceMethod]; /* { dg-warning "is deprecated" } */ 
}
