/* Test basic Objective-C foreach syntax.  This tests iterations that
   do nothing.
*/
/* { dg-do run } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../objc-obj-c++-shared/Object1.m" } */
/* { dg-options "-Wall" } */

#import "../objc-obj-c++-shared/Object1.h"

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
@end

int main (void)
{
  int test_variable = 0;
  int counter = 0;
  id array = nil;
  id object = nil;

  /* Test that 'for (object in array)' is recognized and that nothing
     happens if array is nil.  */
  for (object in array)
    test_variable = 8;

  if (test_variable == 8)
    abort ();

  if (object != nil)
    abort ();

  /* Test that if nothing is done, object is set to nil.  */
  object = [Object new];

  for (object in array)
    ;

  if (object != nil)
    abort ();

  /* Test that you can reference 'object' inside the body.  */
  for (object in array)
    object = nil;

  if (object != nil)
    abort ();

  /* Test that 'for (id element in array) is recognized (and works).  */
  for (id element in array)
    test_variable = 8;

  if (test_variable == 8)
    abort ();

  /* Test that you can reference 'object' inside the body.  */
  for (id element in array)
    element = nil;

  /* Test that C for loops still work.  */
  test_variable = 0;

  for (counter = 0; counter < 4; counter++)
    test_variable++;

  if (test_variable != 4)
    abort ();

  return 0;
}
