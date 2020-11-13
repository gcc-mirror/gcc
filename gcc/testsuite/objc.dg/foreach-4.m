/* Test basic Objective-C foreach syntax.  This tests iterations, with
   the declaration syntax 'for (id object in array) statements'
*/
/* { dg-do run } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */
/* { dg-additional-sources "../objc-obj-c++-shared/nsconstantstring-class-impl.m" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#import "../objc-obj-c++-shared/TestsuiteObject.m"
#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#else
#include "../objc-obj-c++-shared/nsconstantstring-class.h"
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

int main (void)
{
  MyArray *array;
  int test_variable, counter, i;
  id *objects;

  array = [[MyArray alloc] initWithLength: 0
			   objects: NULL];

  /* Test that an empty array does nothing.  */
  for (id object in array)
    abort ();

  /* Test iterating over 1 object.  */
  objects = malloc (sizeof (id) * 1);
  objects[0] = @"One Object";

  array = [[MyArray alloc] initWithLength: 1
			   objects: objects];
  
  for (id object in array)
    printf ("%p\n", object);
  
  /* Test iterating over 20 objects.  */
  objects = malloc (sizeof (id) * 20);
  for (i = 0; i < 20; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 20
			   objects: objects];
  
  for (id object in array)
    printf ("%p\n", object);

  /* Test iterating over 200 objects.  */
  objects = malloc (sizeof (id) * 200);
  for (i = 0; i < 200; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 200
			   objects: objects];
  
  counter = 0;
  for (id object in array)
    {
      if (object != nil)
	counter++;
    }

  if (counter != 200)
    abort ();

  printf ("Counter was %d (should be 200)\n", counter);

  /* Test iterating again over the same array.  */
  counter = 0;
  for (id object in array)
    {
      if (object != nil)
	counter++;
    }

  if (counter != 200)
    abort ();

  printf ("Counter was %d (should be 200)\n", counter);

  /* Test nested iterations.  */
  objects = malloc (sizeof (id) * 20);
  for (i = 0; i < 20; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 20
			   objects: objects];
  counter = 0;
  for (id object in array)
    {
      for (id another_object in array)
	if (another_object != nil)
	  counter++;
    }

  printf ("Counter was %d (should be 400)\n", counter);

  if (counter != 400)
    abort ();

  /* Test 'continue'.  */
  objects = malloc (sizeof (id) * 20);
  for (i = 0; i < 20; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 20
			   objects: objects];
  counter = 0;
  for (id object in array)
    {
      if (counter == 15)
	continue;

      counter++;
    }

  printf ("Counter was %d (should be 15)\n", counter);

  if (counter != 15)
    abort ();

  /* Test 'break'.  */
  objects = malloc (sizeof (id) * 20);
  for (i = 0; i < 20; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 20
			   objects: objects];
  counter = 0;
  for (id object in array)
    {
      counter++;

      if (counter == 15)
	break;
    }

  printf ("Counter was %d (should be 15)\n", counter);

  if (counter != 15)
    abort ();

  /* Test 'break' and 'continue' in nested iterations.  */
  objects = malloc (sizeof (id) * 20);
  for (i = 0; i < 20; i++)
    objects[i] = @"object";
  
  array = [[MyArray alloc] initWithLength: 20
			   objects: objects];
  counter = 0;
  for (id object in array)
    {
      int local_counter = 0;

      /* Each internal loop should increase counter by 24.  */
      for (id another_object in array)
	{
	  local_counter++;
	  
	  if (local_counter == 10)
	    {
	      counter = counter + 20;
	      break;
	    }

	  if (local_counter >= 5)
	    continue;

	  counter++;
	}

      /* Exit after 4 iterations.  */
      if (counter == 96)
	break;
    }

  printf ("Counter was %d (should be 96)\n", counter);

  if (counter != 96)
    abort ();

  /* Test that C for loops still work.  */
  test_variable = 0;

  for (counter = 0; counter < 4; counter++)
    test_variable++;

  if (test_variable != 4)
    abort ();

  return 0;
}
