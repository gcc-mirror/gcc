/* Test for sending messages to aliased classes (and instances thereof).  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

@interface Int1: TestsuiteObject
+ (int) classMeth;
- (int) instanceMeth;
@end

@interface Int2: TestsuiteObject
+ (int) classMeth;      
- (int) instanceMeth;
@end

@implementation Int1
+ (int) classMeth { return 345; }
- (int) instanceMeth { return 697; }
@end

@implementation Int2
+ (int) classMeth { return 1345; }
- (int) instanceMeth { return 1697; }
@end

typedef Int1 Int1Typedef;
@compatibility_alias Int1Alias Int1Typedef;
@compatibility_alias Int2Alias Int2;
typedef Int2Alias Int2Typedef;                  

int main(void) {
  Int1Alias *int1alias = [[Int1Typedef alloc] init];
  Int2Typedef *int2typedef = [[Int2Alias alloc] init];

  CHECK_IF([Int1Typedef classMeth] == 345 && [Int2Alias classMeth] == 1345);
  CHECK_IF([int1alias instanceMeth] == 697 && [int2typedef instanceMeth] == 1697);
  CHECK_IF([(Int2Typedef *)int1alias instanceMeth] == 697);
  CHECK_IF([(Int1Alias *)int2typedef instanceMeth] == 1697);
  return 0;
}

