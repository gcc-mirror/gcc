/* Test if context-sensitive "in", "out", "byref", etc., qualifiers can be
   used as method selectors.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do compile } */

@interface Foo
- (void)insertNewButtonImage:(Foo *)newButtonImage in:(Foo *)buttonCell;
+ (oneway void)oneway:(int)i2 byref:(int)i3 out:(float)f4 bycopy:(float)f5;
@end

@implementation Foo
- (void)insertNewButtonImage:(Foo *)newButtonImage in:(Foo *)buttonCell { }
+ (oneway void)oneway:(int)i2 byref:(int)i3 out:(float)f4 bycopy:(float)f5 { }
@end

/* { dg-final { scan-assembler "insertNewButtonImage:in:" } } */
/* { dg-final { scan-assembler "oneway:byref:out:bycopy:" } } */
