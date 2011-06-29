/* Test that compiling for the GNU runtime works (regardless of
   the system runtime used).  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <stdlib.h>

@interface FooBar: TestsuiteObject
- (void)boo;
@end

int called = 0;

@implementation FooBar
- (void)boo
{
  called ++;
}
@end

int main ()
{
  id fooBarInst = [[FooBar alloc] init];
  [fooBarInst boo];
  if (called != 1)
    abort ();
  return 0;
}
