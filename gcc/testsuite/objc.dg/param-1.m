/* Test if compiler detects object as an parameter to a method
   or not. It is not valid.  */
/* { dg-do compile } */

@interface foo
@end

@implementation foo
@end

@interface bar
-(void) my_method:(foo) my_param; /* { dg-error "can not use an object as parameter to a method" } */
@end

@implementation bar
-(void) my_method:(foo) my_param /* { dg-error "can not use an object as parameter to a method" } */
{
}
@end

