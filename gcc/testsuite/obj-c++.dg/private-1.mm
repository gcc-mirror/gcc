/* Test errors for accessing @private and @protected variables.  */
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
  _private = 12;   /* Ok  */
  _protected = 12; /* Ok  */
  _public = 12;    /* Ok  */
}
@end


@interface MyClass : MySuperClass 
@end

@implementation MyClass
- (void) test
{
  /* Private variables simply don't exist in the subclass.  */
  _private = 12; /* { dg-error "._private. was not declared in this scope" } */

  _protected = 12; /* Ok  */
  _public = 12;    /* Ok  */
}
@end

int main (void)
{
  MyClass *m = nil;
  
  if (m != nil)
    {
      int access;

      access = m->_private;   /* { dg-error "is @private" }  */
      access = m->_protected; /* { dg-error "is @protected" }  */
      access = m->_public;    /* Ok  */
    }

  return 0;
}
