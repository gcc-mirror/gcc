/* Check if casting the receiver type causes method lookup to succeed.  This was broken
   in Objective-C++.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do compile } */

@interface A
@end

@interface B: A
- (void)f;
@end

void g(A *p) { [(B *)p f];  }

