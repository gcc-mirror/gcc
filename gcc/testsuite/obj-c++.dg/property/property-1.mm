/* This program tests use of property provided setter/getter functions. */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../../objc-obj-c++-shared/Object1.mm" } */

#import "../../objc-obj-c++-shared/Object1.h"

@interface Bar : Object
{
  int iVar;
}
@property (setter=MySetter:) int FooBar;
@end

@implementation Bar
@synthesize FooBar=iVar;

- (void) MySetter : (int) value { iVar = value; }

@end

int main(int argc, char *argv[]) {
    Bar *f = [Bar new];
    f.FooBar = 1;

    f.FooBar += 3;

    f.FooBar -= 4;
    return f.FooBar;
}

