/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do compile } */

#include <objc/objc.h>

enum type1;
struct type2;

#if defined(__has_attribute) && __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
@interface MyObject
- (void) method1: (enum type1)argument;
- (void) method2: (struct type2)argument;
@end

@implementation MyObject
- (void) method1: (enum type1)argument { /* { dg-error "does not have a known size" } */
  return;
}
- (void) method2: (struct type2)argument { /* { dg-error "does not have a known size" } */
  return;
}
@end

