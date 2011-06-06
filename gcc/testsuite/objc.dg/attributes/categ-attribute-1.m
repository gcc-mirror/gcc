/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/TestsuiteObject.h"

@interface obj : TestsuiteObject { 
@public 
  int var; 
} 
- (int) mth;
@end

@implementation obj
- (int) mth {  return var; } 
@end

__attribute ((deprecated)) 
@interface obj (dep_categ) 
- (int) depmth;/* { dg-warning "category attributes are not available in this version" } */
@end

@implementation obj (dep_categ)
- (int) depmth { return var + 1; }
@end

int foo (void)
{
    obj *p = [obj new];	 
    int q = [p depmth];
    return [p mth];    
}
