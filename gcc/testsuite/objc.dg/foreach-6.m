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

int main (void)
{
  id array = nil;
  id object = nil;

  for (object in array) /* Ok */
    ;

  for (object in nil) /* Ok */
    ;

  for (object in) /* { dg-error "missing collection in fast enumeration" } */
    ;

  for (object = nil in array) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ;

  for (object in [object enumerator]) /* Ok */
    ;

  for (12 in array) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ; /* { dg-error "iterating variable in fast enumeration is not an object" } */

  for (object in 12)
    ; /* { dg-error "collection in fast enumeration is not an object" } */

  return 0;
}
