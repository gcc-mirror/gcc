/* Test for handling of struct-returning methods.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

struct astruct {
  float a, b;
} globa = { 1.0, 2.0 };

struct bstruct {
  float a, b, c, d, e, f;
} globb = { 1, 2, 3, 4, 5, 6 };

@interface foo : TestsuiteObject
- (struct astruct) stret;
- (struct bstruct) stretb;
@end

@implementation foo : TestsuiteObject
- (struct astruct) stret { return globa; }
- (struct bstruct) stretb { return globb; }
@end

@interface bar: foo
- (struct astruct) stret;
- (struct bstruct) stretb;
@end

@implementation bar
- (struct astruct) stret { struct astruct a = [super stret]; a.b = 77; return a; }
- (struct bstruct) stretb { struct bstruct b = [super stretb]; b.e = 99; return b; }
@end

int main(void)
{
  foo *obj = [foo new];
  bar *obj2 = [bar new];
  struct astruct loc, loc2;
  struct bstruct locb, locb2;

  loc = [obj stret];
  CHECK_IF(loc.a == 1.0 && loc.b == 2.0);

  locb = [obj stretb];
  CHECK_IF(locb.f == 6 && locb.c == 3);
  CHECK_IF(locb.e == 5 && locb.b == 2);
  CHECK_IF(locb.d == 4 && locb.a == 1);

  loc2 = [obj2 stret];
  CHECK_IF(loc2.a == 1.0 && loc2.b == 77);
  
  locb2 = [obj2 stretb];
  CHECK_IF(locb2.f == 6 && locb2.c == 3);
  CHECK_IF(locb2.e == 99 && locb2.b == 2);
  CHECK_IF(locb2.d == 4 && locb2.a == 1);
  
  return 0;
}

