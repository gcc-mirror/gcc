/* Test for -Wundeclared-selector.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */
/* { dg-options "-Wundeclared-selector" } */

#include <objc/objc.h>

@interface MyClass

+ (void) methodA;
- (void) methodB;
+ (void) methodD;
- (void) methodF;

@end

@implementation MyClass

+ (void) methodA {}
- (void) methodB {}
+ (void) methodD
{
  SEL d = @selector(methodD); /* Ok */
  SEL e = @selector(methodE); /* { dg-warning "undeclared selector" } */
}

- (void) methodE
{
  SEL e = @selector(methodE); /* Ok */
}

- (void) methodF
{
  SEL e = @selector(methodE); /* Ok */
}

@end

int main (void)
{
  SEL a = @selector(methodA); /* Ok */
  SEL b = @selector(methodB); /* Ok */
  SEL c = @selector(methodC); /* { dg-warning "undeclared selector" } */
  SEL d = @selector(methodD); /* Ok */
  SEL e = @selector(methodE); /* Ok */
  return 0;
  
}
