/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/TestsuiteObject.h"

@interface obj : TestsuiteObject {
@public 
  int var; 
} 
- (int) mth;
+ (id) dep_cls_mth __attribute__((deprecated)) ;
- (int) dep_ins_mth __attribute__((deprecated)) ;
- (int) dep_ins_mtharg: (int) i __attribute__((deprecated)) ;
- (int) dep_ins_mtharg1: (int) i __attribute__((deprecated)) add: (int) j;/* { dg-error "method attributes must be specified at the end " } */
- (int) nodef __attribute__((deprecated)) { return var-2; } ; /* { dg-error "expected ';' before '\{' token" } */
__attribute__((deprecated))
- (int) bad_pref_mth; /* { dg-warning "prefix attributes are ignored for methods" } */
@end

@implementation obj
- (int) mth { return var; }
+ (id) dep_cls_mth { return self; }
- (int) dep_ins_mth  { return var ; }
- (int) dep_ins_mtharg: (int) i { return var + i ; }
- (int) dep_ins_mtharg1: (int) i add: (int) j { return var + i + j ; } 
- (int) bad_pref_mth { return var; };
- (int) nodef { return var-2; } ; 
@end 

int foo (void)
{
  obj *p = [obj new];
  id n = [obj dep_cls_mth];	/* { dg-warning "is deprecated" } */
  
  [p dep_ins_mth];		/* { dg-warning "is deprecated" } */
  [p dep_ins_mtharg:2];		/* { dg-warning "is deprecated" } */
  [p dep_ins_mtharg1:3 add:3];
	
  return [p mth];
}
