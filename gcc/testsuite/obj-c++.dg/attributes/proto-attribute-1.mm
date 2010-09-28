/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/Object1.h"

__attribute ((deprecated)) 
@protocol dep_proto /* { dg-warning "protocol attributes are not available in this version" } */
- (int) depprotomth; 
@end

@interface obj : Object <dep_proto>
{ 
@public 
  int var; 
} 
- (int) mth;
@end

@implementation obj
- (int) mth {  return var; } 
- (int) depprotomth { return var + 1; }
@end

int foo (void)
{
    obj *p = [obj new];	 
    int q = [p depprotomth];
    return [p mth];    
}
