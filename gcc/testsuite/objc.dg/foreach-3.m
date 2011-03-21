/* Test basic Objective-C foreach syntax.  This tests the mutation.
*/
/* { dg-do compile } */

/* FIXME: This test should be run, and it succeeds if the program
   aborts at the right time (when the mutation happens).  It currently
   works, but how do we tell the testsuite to test for it ?
*/

#import "../objc-obj-c++-shared/Object1.h"
#import "../objc-obj-c++-shared/next-mapping.h"
#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#endif

extern int printf (const char *, ...);
#include <stdlib.h>

/*
struct __objcFastEnumerationState
{
  unsigned long state;
  id            *itemsPtr;
  unsigned long *mutationsPtr;
  unsigned long extra[5];
};
*/

 /* A mini-array implementation that can be used to test fast
    enumeration.  You create the array with some objects; you can
    mutate the array, and you can fast-enumerate it.
 */
@interface MyArray : Object
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

@implementation MyArray : Object
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

  /* Change the mutationsPtr if 'mutate' is called.  */
  state->mutationsPtr = &mutated;

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

  return batch_size;
}
@end

int main (void)
{
  MyArray *array;
  Object *object;
  int counter, i;
  id *objects;

  /* Test iterating over 20 objects, mutating after 15.  */
  objects = malloc (sizeof (id) * 20);
  for (i = 0; i < 20; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 20
			   objects: objects];
  
  counter = 0;
  for (object in array)
    {
      counter++;
      printf ("%d\n", counter);
      if (counter == 14)
	{
	  printf ("Mutating (should abort at next iteration)\n");
	  [array mutate];
	}
    }

  return 0;
}
