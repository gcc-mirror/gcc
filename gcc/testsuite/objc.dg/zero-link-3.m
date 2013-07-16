/* Check that the '-fzero-link' flag doesn't prevent messaging from working. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run { target *-*-darwin* } } */
/* { dg-additional-options "-fzero-link" } */
/* { dg-additional-options "-framework Foundation" { target { *-*-darwin* } } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#ifdef __NEXT_RUNTIME__
#include <Foundation/NSObject.h>
#define OBJECT NSObject
#else
#include <objc/Object.h>
#include <objc/Protocol.h>
#define OBJECT Object
#endif

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort();

@interface Base: OBJECT
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

