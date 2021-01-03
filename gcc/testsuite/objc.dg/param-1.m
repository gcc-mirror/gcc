/* Test if compiler detects object as an parameter to a method
   or not. It is not valid.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface foo
@end

@implementation foo
@end

@interface bar
-(void) my_method:(foo) my_param; /* { dg-error "cannot use an object as parameter to a method" } */
@end

@implementation bar
-(void) my_method:(foo) my_param /* { dg-error "cannot use an object as parameter to a method" } */
{
}
@end

