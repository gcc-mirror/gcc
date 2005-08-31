/* Test that compiling for the GNU runtime works (regardless of
   the system runtime used).  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do run } */
/* { dg-options "-fgnu-runtime" } */

#include <objc/Object.h>

@interface FooBar: Object
- (void)boo;
@end

int called = 0;
extern "C" void abort ();

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
