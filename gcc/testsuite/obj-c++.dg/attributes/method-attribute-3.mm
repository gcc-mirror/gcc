/* { dg-do compile } */

#include "../../objc-obj-c++-shared/Object1.h"

@interface obj : Object {
@public 
  int var;
}
- (int) vargsn: (int) count, ... __attribute__((deprecated)); /* { dg-warning " method attributes are not available in this version of the compiler" } */
@end

@implementation obj
- (int) vargsn: (int) count, ... 
{
  return 0;
}
@end 

int foo (void)
{
  obj *p = [obj new];
  
  return [p vargsn:0];
}
