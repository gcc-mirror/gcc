/* Test to make sure that file name does not appear in the binary. */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include <objc/objc.h>

@interface Foo { Class isa; } @end
@implementation Foo @end

@interface Bar : Foo { Class Barisa; } @end

@implementation Bar : Foo @end;

@interface FINAL : Bar { Class FINALisa; } @end

@implementation FINAL : Bar @end;

int main(int argc, char **argv)
{
     return 0;
}

/* { dg-final { scan-assembler-not "objc-nofilename-1.m" } } */
