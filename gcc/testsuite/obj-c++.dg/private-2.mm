/* Test warnings for shadowing instance variables.  */
/* Based on work by: Nicola Pero <nicola@brainstorm.co.uk>.  */

/* { dg-do compile } */

#include <objc/objc.h>

@interface MySuperClass
{
@private
  int _private;

@protected
  int _protected;

@public
  int _public;
}
- (void) test;
@end

@implementation MySuperClass
- (void) test
{
  /* FIXME: I wonder if the warnings shouldn't be better generated
     when the variable is declared, rather than used!  */
  int _private = 12;
  int _protected = 12;
  int _public = 12;
  int a;
  
  a = _private;    /* { dg-warning "hides instance variable" } */
  a = _protected;  /* { dg-warning "hides instance variable" } */
  a = _public;     /* { dg-warning "hides instance variable" } */
}
@end


@interface MyClass : MySuperClass 
@end

@implementation MyClass
- (void) test
{
  int _private = 12;
  int _protected = 12;
  int _public = 12;
  int a;

  /* The private variable can be shadowed without warnings, because
   * it's invisible, and not accessible, to the subclass!  */
  a = _private;   /* Ok  */
  a = _protected; /* { dg-warning "hides instance variable" } */
  a = _public;    /* { dg-warning "hides instance variable" } */
}
@end
