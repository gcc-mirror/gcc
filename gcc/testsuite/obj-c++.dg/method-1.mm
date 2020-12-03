/* Test whether casting 'id' to a specific class removes method lookup
   ambiguity.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile } */
// { dg-additional-options "-Wno-objc-root-class" }

#include <objc/objc.h>

@class Int1, Int2;

@interface Int1
+ (Int1 *)classMethod1;
+ (id)classMethod2;
- (Int1 *)instanceMethod:(Int2 *)arg;  /* { dg-bogus "using" } */
@end

@interface Int2: Int1
+ (Int1 *)classMethod1; 
+ (id)classMethod2;
- (id)int2Method;
- (int)instanceMethod:(int)arg;  /* { dg-bogus "also found" } */
@end

int main(void) {
  id i = [(Int2 *)[Int1 classMethod1] int2Method]; /* { dg-line i_def } */
	/* { dg-bogus "may not respond to" "" { target *-*-* } i_def } */
  int j = [(Int2 *)[Int2 classMethod2] instanceMethod: 45];  /* { dg-bogus "multiple methods" } */
	/* { dg-bogus "invalid conversion" "" { target *-*-* } i_def } */
	/* { dg-bogus "invalid conversion" "" { target *-*-* } i_def } */
  return j;
}
