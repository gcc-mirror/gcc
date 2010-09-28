/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/Object1.h"

__attribute ((deprecated)) 
@interface depobj : Object { /* { dg-warning "class attributes are not available in this version" } */
@public 
  int ivar; 
} 
- (int) mth;
@end

__attribute ((deprecated)) 
@implementation depobj /* { dg-warning "prefix attributes are ignored for implementations" } */
-(int) mth {  return ivar; } 
@end

int foo (void)
{
    depobj *p = [depobj new];	/*  dg - warning "deprecated"   */ 

    int q = p->ivar;
    return [p mth];    
}
