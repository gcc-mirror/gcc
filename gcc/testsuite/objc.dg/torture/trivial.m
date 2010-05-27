/* { dg-do run } */
/* { dg-skip-if "PR44140" { *-*-* } { "-flto" "-fwhopr" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../../objc-obj-c++-shared/Object1.m" } */

#import "../../objc-obj-c++-shared/Object1.h"

int main(void)
{
  [Object class];
  return 0;
}
