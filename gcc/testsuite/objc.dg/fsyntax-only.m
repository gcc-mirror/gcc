/* Test -fsyntax-only compiler option  */
/* { dg-do compile } */
/* { dg-options "-fsyntax-only" } */

#if defined(__has_attribute) && __has_attribute(objc_root_class)
__attribute__((objc_root_class))
#endif
@interface foo
-(void) my_method:(int) i with:(int) j;
@end

@implementation foo
-(void) my_method:(int) i with:(int) j { }
@end
