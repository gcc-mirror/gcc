/* Test assignments and comparisons involving `one-off' protocols.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */
#include <objc/objc.h>

@protocol MyProtocol
- (void) method;
@end

@interface MyClass
@end

int main()
{
  id obj = nil;
  id <MyProtocol> obj_p = nil;
  MyClass<MyProtocol> *obj_cp = nil;

  obj_cp = obj; /* Ok */
  obj = obj_cp; /* Ok */

  obj_cp = obj_p; /* { dg-warning "incompatible pointer type" } */
  obj_p = obj_cp; /* Ok */ /* Spurious 2.95.4 warning here.  */
  
  if (obj_cp == obj) ; /* Ok */
  if (obj == obj_cp) ; /* Ok */

  if (obj_cp == obj_p) ; /* Ok */
  if (obj_p == obj_cp) ; /* Ok */

  return 0;
}
