/* Test Objective-C capability for handling GNU/C99 designated
   initializers, and distinguishing them from message sends.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-std=gnu99" } */
/* { dg-do run } */

#include <stdio.h>           
#include <objc/objc.h>
#include <objc/Object.h>

@interface Cls : Object
+ (int) meth1;
+ (int) meth2;
+ (void) doTests;
@end

@implementation Cls
+ (int) meth1 { return 45; }
+ (int) meth2 { return 21; }
+ (void) doTests {
  int arr[6] = { 
    0, 
    [Cls meth1], 
    [2 + 1] = 3, 
    [2 * 2 ... 5] = [0 meth2], /* { dg-warning "invalid receiver type" } */ 
    [2] [Cls meth2]
  };

  if (arr[0] != 0 || arr[1] != 45 || arr[2] != 21 || arr[3] != 3)
    abort (); /* { dg-warning "implicit declaration" } */

  printf ("%s\n", [super name]);
  printf ("%d %d %d %d %d %d\n", arr[0], arr[1], arr[2], arr[3], arr[4], arr[5]);
}
@end

int main(void) {
  [Cls doTests];
  return 0;
}
