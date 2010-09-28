/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/Object1.h"

/* Normal deprecated func. */
__attribute ((deprecated)) void f1();
__attribute__ ((deprecated("use some new func"))) void f2();

__attribute__ ((deprecated)) 
@interface depobj : Object { /* { dg-warning "class attributes are not available in this version" } */
@public 
  int var; 
} 
- (int) mth;
@end

@implementation depobj
-(int) mth {  return var; } 
@end

@interface depobj (ok_categ) 
@end 

@interface NS : depobj 
@end 

depobj * deprecated;

int foo (depobj *dep_obj) /*  dg - warning "deprecated"  */
{
    depobj *p = [depobj new];	/*  dg - warning "deprecated"   */ 

    f1();	/* { dg-warning "'void f1..' is deprecated .declared at" } */
    f2();	/* { dg-warning "'void f2..' is deprecated .declared at \[^\\)\]*.: use some new func" } */
    int q = p->var;
    return [p mth];    
}
