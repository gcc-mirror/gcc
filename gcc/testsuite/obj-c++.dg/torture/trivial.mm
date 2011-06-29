// { dg-do run }

// { dg-xfail-run-if "OBJC2 runtime" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime"  } { "-fgnu-runtime" } }

#import "../../objc-obj-c++-shared/TestsuiteObject.m"

int main(void)
{
  [TestsuiteObject class];
  return 0;
}
