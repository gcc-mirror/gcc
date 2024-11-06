/* Test basic Objective-C foreach syntax.  This tests iterations, with
   the basic syntax 'for (object in array) statements'
*/
/* { dg-do run } */
/* { dg-skip-if "No NeXT fast enum. pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */
/* { dg-additional-sources "../objc-obj-c++-shared/nsconstantstring-class-impl.m" } */
/* { dg-additional-options "-Wno-objc-root-class" } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
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

void
foo (MyArray *array, int x)
{
  TestsuiteObject *object;
  int i, j, k;

 label1:
  for (i = 0; i < 2; ++i)
    {
      if (i == 1)
	{
	  if (x != 5)
	    abort ();
	  return;
	}
      k = 0;
     label2:
      for (object in array)
	{
	  if (k == 1)
	    {
	      if (x != 3)
		abort ();
	      return;
	    }
	  ++k;
	 label3:
	  for (j = 0; j < 2; ++j)
	    {
	      if (j == 1)
		{
		  if (x != 1)
		    abort ();
		  return;
		}
	     label4:
	      switch (x)
		{
		case 0:
		  break label4;
		case 1:
		  continue label3;
		case 2:
		  break label3;
		case 3:
		  continue label2;
		case 4:
		  break label2;
		case 5:
		  continue label1;
		default:
		  break label1;
		}
	      if (x != 0)
		abort ();
	      return;
	    }
	  if (x != 2)
	    abort ();
	  return;
	}
      if (x != 4)
	abort ();
      return;
    }
  if (x <= 5)
    abort ();
}

int
main ()
{
  MyArray *array;
  id objects[2] = { @"object1", @"object2" };
  int i;

  array = [[MyArray alloc] initWithLength: 2
			   objects: objects];
  for (i = 0; i < 6; ++i)
    foo (array, i);

  return 0;
}
