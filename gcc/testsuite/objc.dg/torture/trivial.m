/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#import "../../objc-obj-c++-shared/TestsuiteObject.m"

int main(void)
{
  [TestsuiteObject class];
  return 0;
}
