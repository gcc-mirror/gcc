/* { dg-do compile } */

#include <objc/objc.h>

@class Foo;

@protocol Bar

- (void)bang;

@end

void foo()
{
    Foo<Bar> *foo = nil;
    [foo bang];
}

