/* Test Objective-C capability for handling GNU/C99 designated initializers, and distinguishing them
   from message sends.  Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-std=gnu99" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */


#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdio.h> 
#include <stdlib.h>

@interface Cls : TestsuiteObject
+ (int) meth1;
+ (int) meth2;
+ (void) doTests;
@end

@implementation Cls
+ (int) meth1 { return 45; }
+ (int) meth2 { return 21; }
+ (void) doTests {
  int arr[7] = { 
    0, 
    [Cls meth1], 
    [2 + 1] = 3, 
    [2 * 2 ... 5] = (size_t)[0 meth4], /* { dg-warning "invalid receiver type" } */ 
       /* { dg-warning "no .\\-meth4. method found" "" { target *-*-* } .-1 } */
    [2] [Cls meth2],
    /* Since invalid receivers are treated as 'id' for purposes of message
       lookup, we _should_ find a meth2 to call below.  */
    [6] = (int)[0 meth2] /* { dg-warning "invalid receiver type" } */
  };

  if (arr[0] != 0 || arr[1] != 45 || arr[2] != 21 || arr[3] != 3)
    abort ();

  printf ("%s\n", [super name]);
  printf ("%d %d %d %d %d %d\n", arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]);
}
@end

int main(void) {
  [Cls doTests];
  return 0;
}

/* { dg-warning "Messages without a matching method signature" "" { target *-*-* } 0 } */
/* { dg-warning "will be assumed to return .id. and accept" "" { target *-*-* } 0 } */
/* { dg-warning ".\.\.\.. as arguments" "" { target *-*-* } 0 } */


