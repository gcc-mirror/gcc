/* Test errors for accessing @private and @protected variables.  */
/* Author: Nicola Pero <nicola@brainstorm.co.uk>.  */
/* { dg-do compile } */
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
  private = 12;   /* Ok  */
  protected = 12; /* Ok  */
  public = 12;    /* Ok  */
}
@end


@interface MyClass : MySuperClass 
@end

@implementation MyClass
- (void) test
{
  /* Private variables simply don't exist in the subclass.  */
  private = 12;  /* { dg-error "instance variable" } */
  /* { dg-message "function it appears in" "" { target *-*-* } .-1 } */

  protected = 12; /* Ok  */
  public = 12;    /* Ok  */
}
@end

int main (void)
{
  MyClass *m = nil;
  
  if (m != nil)
    {
      int access;

      access = m->private;   /* { dg-warning "is @private" }  */
      access = m->protected; /* { dg-warning "is @protected" }  */
      access = m->public;    /* Ok  */
    }

  return 0;
}
