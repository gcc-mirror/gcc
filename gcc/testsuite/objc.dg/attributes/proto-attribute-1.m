/* { dg-do compile } */

#include <objc/objc.h>

__attribute ((deprecated)) 
@protocol dep_proto 
- (int) depprotomth;
@end

@interface obj <dep_proto> /* { dg-warning "is deprecated" } */
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
