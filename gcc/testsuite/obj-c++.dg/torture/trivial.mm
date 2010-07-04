// { dg-do run }

// { dg-skip-if "PR44140" { *-*-* } { "-flto" "-fwhopr" } { "" } }
// { dg-xfail-run-if "OBJC2 runtime" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime"  } { "-fgnu-runtime" } }
// { dg-additional-sources "../../objc-obj-c++-shared/Object1.mm" }

#import "../../objc-obj-c++-shared/Object1.h"

int main(void)
{
  [Object class];
  return 0;
}
