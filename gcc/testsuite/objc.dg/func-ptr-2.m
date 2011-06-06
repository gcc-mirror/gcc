/* Check if method parameters that are functions are gracefully decayed
   into pointers.  */
/* Contributed by Ziemowit Laski  <zlaski@apple.com>  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <stdlib.h>
#include "../objc-obj-c++-shared/TestsuiteObject.m"

@interface Func: TestsuiteObject
+ (int) processNumber:(int)a and:(int)b usingFunction:(int(int,int))func;
@end

@implementation Func
+ (int) processNumber:(int)a and:(int)b usingFunction:(int(int,int))func {
  return func (a, b);
}
@end

static int my_computation(int a, int b) {
  return a * 2 + b * 3;
}

static int processNumber(int a, int b, int func(int, int)) {
  return func(a, b);
}

int main(void) {
  int result = processNumber (6, 8, my_computation);
  if (result != 36)
    abort ();

  result = [Func processNumber:8 and:6 usingFunction:my_computation];
  if (result != 34)
    abort ();

  return 0;
}

