/* { dg-do compile } */

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/Object1.h"

@interface obj : Object {
@public 
  int var; 
} 
- (int) depmtharg:(int) iarg __attribute__((deprecated)); /* { dg-warning "method attributes are not available in this version" } */
- (int) unusedarg:(int) __attribute__((unused)) uarg ; /* { dg-warning "method parameter attributes are not available in this version" } */
- (int) depunusedarg:(int) __attribute__((unused)) uarg __attribute__((deprecated)) ; /* { dg-warning "method attributes are not available in this version" } */
				/* { dg-warning "method parameter attributes are not available in this version" "" { target *-*-* } 12 } */
@end

@implementation obj
- (int) depmtharg:(int) iarg { return var + iarg ; };
- (int) unusedarg:(int) __attribute__((unused)) uarg { return var; } ; /* { dg-warning "method parameter attributes are not available in this version" } */
- (int) depunusedarg:(int) __attribute__((unused)) uarg { return var; }; /* { dg-warning "method parameter attributes are not available in this version" } */
@end 

int foo (void)
{
  obj *p = [obj new];
  
  [p depmtharg:1];
  [p unusedarg:2];
  [p depunusedarg:3 ];

  return [p depmtharg:0];    
}
