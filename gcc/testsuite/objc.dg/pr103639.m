/* PR objc/103639 */
/* { dg-do run } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../objc-obj-c++-shared/nsconstantstring-class-impl.m" } */
/* { dg-additional-options "-mno-constant-cfstrings" { target *-*-darwin* } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#import "../objc-obj-c++-shared/TestsuiteObject.m"
#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#else
#include "../objc-obj-c++-shared/nsconstantstring-class.h"
#endif

extern int printf (const char *, ...);
#include <stdlib.h>

/* A mini-array implementation that can be used to test fast
    enumeration.  You create the array with some objects; you can
    mutate the array, and you can fast-enumerate it.
 */
@interface MyArray : TestsuiteObject
{
  unsigned int length;
  id *objects;
  unsigned long mutated;
}
- (id) initWithLength: (unsigned int)l  objects: (id *)o;
- (void) mutate;
- (unsigned long)countByEnumeratingWithState: (struct __objcFastEnumerationState *)state
                                     objects:(id *)stackbuf 
                                       count:(unsigned long)len;
@end

@implementation MyArray : TestsuiteObject
- (id) initWithLength: (unsigned int)l
	      objects: (id *)o
{
  length = l;
  objects = o;
  mutated = 0;
  return self;
}
- (void) mutate
{
  mutated = 1;
}
- (unsigned long)countByEnumeratingWithState: (struct __objcFastEnumerationState*)state 
		  		     objects: (id*)stackbuf
			 	       count: (unsigned long)len
{
  unsigned long i, batch_size;

  /* We keep how many objects we served in the state->state counter.  So the next batch
     will contain up to length - state->state objects.  */
  batch_size = length - state->state;

  /* Make obvious adjustments.  */
  if (batch_size < 0)
    batch_size = 0;

  if (batch_size > len)
    batch_size = len;

  /* Copy the objects.  */
  for (i = 0; i < batch_size; i++)
    stackbuf[i] = objects[i];

  state->state += batch_size;
  state->itemsPtr = stackbuf;
  state->mutationsPtr = &mutated;

  return batch_size;
}
@end

int check = 0;

int
main()
{
  id *objects = malloc (sizeof (id) * 2);
  objects[0] = @"a";
  objects[1] = @"b";

  MyArray *array = [[MyArray alloc] initWithLength: 2 objects: objects];

  int someVar = 0;
  for (id object in array) {
    switch (someVar) {
      case 0:
	break;
    }
    ++check;
  }

  if (check != 2)
    abort ();
  return 0;
}
