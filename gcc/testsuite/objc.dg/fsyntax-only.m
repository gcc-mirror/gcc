/* Test -fsyntax-only compiler option  */
/* { dg-do compile } */
/* { dg-options "-fsyntax-only" } */

@interface foo
-(void) my_method:(int) i with:(int) j;
@end

@implementation foo
-(void) my_method:(int) i with:(int) j { }
@end
