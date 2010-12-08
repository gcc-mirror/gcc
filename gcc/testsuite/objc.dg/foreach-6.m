/* Test basic Objective-C foreach syntax.  This tests warnings and errors.  */
/* { dg-do compile } */

#import "../objc-obj-c++-shared/Object1.h"
#import "../objc-obj-c++-shared/next-mapping.h"

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
- (Class) classEnumerator;
@end

int main (void)
{
  id array = nil;
  id object = nil;
  id *invalid = 0;

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

  for (object in [object classEnumerator]) /* Ok */
    ;

  for (12 in array) /* { dg-error "invalid iterating variable in fast enumeration" } */
    ;               /* { dg-error "iterating variable in fast enumeration is not an object" "" { target *-*-* } 48 } */

  for (object in 12) /* { dg-error "collection in fast enumeration is not an object" } */
    ;

  for (object in invalid) /* { dg-error "collection in fast enumeration is not an object" } */
    ;

  for (invalid in [object enumerator]) /* { dg-error "iterating variable in fast enumeration is not an object" } */
    ;

  return 0;
}
