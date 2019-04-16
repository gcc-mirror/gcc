/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/TestsuiteObject.h"

@interface obj : TestsuiteObject {
@public 
  int var; 
} 
- (int) depmth __attribute__((deprecated)); 
- (int) depmtharg:(int) iarg __attribute__((deprecated)); 
- (int) unusedarg:(int) __attribute__((unused)) uarg ;
- (int) depunusedarg:(int) __attribute__((unused)) uarg __attribute__((deprecated)) ;
@end

@implementation obj
- (int) depmth __attribute__((deprecated)) { return var; }  /* { dg-warning "method attributes cannot be specified in @implementation context" } */
- (int) depmtharg:(int) iarg { return var + iarg ; }
- (int) unusedarg:(int) __attribute__((unused)) uarg { return var; }
- (int) depunusedarg:(int) __attribute__((unused)) uarg { return var; }
@end 

int foo (void)
{
  obj *p = [obj new];
  
  [p depmth];		/* { dg-warning "is deprecated" } */
  [p depmtharg:1];	/* { dg-warning "is deprecated" } */
  [p unusedarg:2];	/* { dg-bogus "is deprecated" } */
  [p depunusedarg:3 ];	/* { dg-warning "is deprecated" } */

  return [p depmtharg:0]; /* { dg-warning "is deprecated" } */   
}
