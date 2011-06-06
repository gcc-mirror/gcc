/* { dg-do compile } */

#include "../../objc-obj-c++-shared/TestsuiteObject.h"

@interface obj : TestsuiteObject {
@public 
  int var;
}
- (int) vargsn: (int) count, ... __attribute__((deprecated));
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
  
  return [p vargsn:0];  /* { dg-warning "'vargsn:' is deprecated .declared at" } */
}
