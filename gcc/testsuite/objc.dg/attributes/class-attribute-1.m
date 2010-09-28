/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/Object1.h"

/* Normal deprecated func. */
__attribute ((deprecated)) void f1();
__attribute__ ((deprecated("use some new func"))) void f2();

__attribute__ ((deprecated)) 
@interface DEPRECATED : Object
  { @public int ivar; } /* { dg-warning "class attributes are not available in this version" } */
  - (int) instancemethod;
@end

@implementation DEPRECATED
-(int) instancemethod {  return ivar; } 
@end

@interface DEPRECATED (Category) 
@end /*  dg - warning "deprecated"  */

@interface NS : DEPRECATED 
@end /* dg - warning "deprecated"  */

DEPRECATED * deprecated_obj; /*  dg - warning "deprecated"  */

int foo (DEPRECATED *unavailable_obj) /*  dg - warning "deprecated"  */
{
    DEPRECATED *p = [DEPRECATED new];	/*  dg - warning "deprecated"   */ 

    f1();	/* { dg-warning "'f1' is deprecated" } */
    f2();	/* { dg-warning "'f2' is deprecated .declared at \[^\\)\]*.: use some new func" } */
    int q = p->ivar;
    return [p instancemethod];    
}
