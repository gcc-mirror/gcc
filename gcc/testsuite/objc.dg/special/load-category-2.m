/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <stdio.h>
#include <stdlib.h>
#include <objc/objc.h>

#include "load-category-2.h"

/* This test tests that +load is called in the correct order for
   classes and categories.  +load needs to be called in superclasses
   before subclasses, and in the main class before categories.  */

/* Compile the classes in random order to prevent the runtime from
   sending +load in the correct order just because the classes happen
   to have been compiled in that order.  */
@implementation TestClass2
+ load
{
  printf ("[TestClass2 +load]\n");
  /* Check superclasses/subclasses +load order.  */
  check_that_load_step_was_completed (0);
  check_that_load_step_was_not_completed (1);
  check_that_load_step_was_not_completed (2);

  /* Check that the corresponding category's +load was not done.  */
  check_that_load_step_was_not_completed (4);

  complete_load_step (1);
}
@end

@implementation TestClass3
+ load
{
  printf ("[TestClass3 +load]\n");

  /* Check superclasses/subclasses +load order.  */
  check_that_load_step_was_completed (0);
  check_that_load_step_was_completed (1);
  check_that_load_step_was_not_completed (2);

  /* Check that the corresponding category's +load was not done.  */
  check_that_load_step_was_not_completed (5);

  complete_load_step (2);
}
@end

@implementation TestClass1
+ initialize { return self; }
+ load
{
  printf ("[TestClass1 +load]\n");

  /* Check superclasses/subclasses +load order.  */
  check_that_load_step_was_not_completed (0);
  check_that_load_step_was_not_completed (1);
  check_that_load_step_was_not_completed (2);

  /* Check that the corresponding category's +load was not done.  */
  check_that_load_step_was_not_completed (3);

  complete_load_step (0);
}
@end


static BOOL load_step_completed[6] = { NO, NO, NO, NO, NO, NO };

void complete_load_step (int load_step)
{
  load_step_completed[load_step] = YES;
}

void check_that_load_step_was_completed (int load_step)
{
  if (load_step_completed[load_step] == NO)
    {
      printf ("Load step %d was not completed but should have been\n", load_step);
      abort ();
    }
}

void check_that_load_step_was_not_completed (int load_step)
{
  if (load_step_completed[load_step] == YES)
    {
      printf ("Load step %d was completed but shouldn't have been\n", load_step);
      abort ();
    }
}

int main (void)
{
  check_that_load_step_was_completed (0);
  check_that_load_step_was_completed (1);
  check_that_load_step_was_completed (2);
  check_that_load_step_was_completed (3);
  check_that_load_step_was_completed (4);
  check_that_load_step_was_completed (5);

  return 0;
}
