/* Check that the '-fzero-link' flag doesn't prevent messaging from working. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-fnext-runtime -fzero-link -lobjc" } */
/* { dg-do run { target *-*-darwin* } } */

#import <objc/objc.h>
#import <objc/Object.h>

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
