/* Test basic Objective-C foreach syntax.  This tests warnings and errors.  */
/* FIXME: Run this test with the NeXT runtime as well.  */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */
/* { dg-do compile } */

#include <objc/objc.h>
#include <objc/Object.h>
extern void abort (void);
/*
struct __objcFastEnumerationState
{
  unsigned long state;
  id            *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
};
*/
@interface Object (NSFastEnumeration)
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

  for (typedef int my_typedef in array) /* { dg-error "declaration of non-variable" } */
    ; /* { dg-error "iterating variable in fast enumeration is not an object" } */

  for (function () in nil) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ; /* { dg-error "iterating variable in fast enumeration is not an object" } */

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

