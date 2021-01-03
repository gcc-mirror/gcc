/* Test warnings for shadowing instance variables.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */
#include <objc/objc.h>

@interface MySuperClass
{
@private
  int private;

@protected
  int protected;

@public
  int public;
}
- (void) test;
@end

@implementation MySuperClass
- (void) test
{
  /* FIXME: I wonder if the warnings shouldn't be better generated
     when the variable is declared, rather than used!  */
  int private = 12;
  int protected = 12;
  int public = 12;
  int a;
  
  a = private;    /* { dg-warning "hides instance variable" } */
  a = protected;  /* { dg-warning "hides instance variable" } */
  a = public;     /* { dg-warning "hides instance variable" } */
}
@end


@interface MyClass : MySuperClass 
@end

@implementation MyClass
- (void) test
{
  int private = 12;
  int protected = 12;
  int public = 12;
  int a;

  /* The private variable can be shadowed without warnings, because
   * it's invisible, and not accessible, to the subclass!  */
  a = private;   /* Ok  */
  a = protected; /* { dg-warning "hides instance variable" } */
  a = public;    /* { dg-warning "hides instance variable" } */
}
@end
