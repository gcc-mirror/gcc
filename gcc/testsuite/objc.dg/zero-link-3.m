/* Check that the '-fzero-link' flag doesn't prevent messaging from working. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-options "-fzero-link" } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"
//#import <objc/objc.h>

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort();

@interface Base: Object
+ (int) getValue;
@end

@implementation Base
+ (int) getValue { return 1593; }
@end

int main(void) {
  int val = [Base getValue];
  CHECK_IF(val == 1593);
  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"
