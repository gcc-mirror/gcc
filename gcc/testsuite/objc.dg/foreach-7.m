/* Test basic Objective-C foreach syntax.  This tests warnings and errors.  */
/* 
   { dg-options "-ftrack-macro-expansion=0" }
   { dg-do compile } 
*/

#import "../objc-obj-c++-shared/TestsuiteObject.h"
#import <objc/objc.h>
#undef  nil
#define nil ((id)0)
/*
struct __objcFastEnumerationState
{
  unsigned long state;
  id            *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
};
*/
@interface TestsuiteObject (NSFastEnumeration)
- (unsigned long)countByEnumeratingWithState: (struct __objcFastEnumerationState *)state
                                     objects:(id *)stackbuf 
                                       count:(unsigned int)len;
- (id) enumerator;
@end

void function (void)
{
  return;
}

id object_function (void)
{
  return nil;
}

int main (void)
{
  id array = nil;
  id object = nil;

  for (typedef int my_typedef in array)
    ;                                   /* { dg-error "iterating variable in fast enumeration is not an object" "" { target *-*-* } .-1 } */

  for (function () in nil) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ;                      /* { dg-error "iterating variable in fast enumeration is not an object" "" { target *-*-* } .-1 } */

  for (object_function () in nil) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ;

  for ([object enumerator] in array) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ;

  for (object = nil in array) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ;

  for (id key, value in array) /* { dg-error "multiple iterating variables in fast enumeration" } */
    ;

  return 0;
}

