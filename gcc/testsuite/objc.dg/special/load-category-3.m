/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* This test is identical to load-category-2, but the classes and
   categories are created in inverted order in the modules, to test
   that you can load classes first, or categories first, and it all
   still works in both cases.  */

#include <stdio.h>
#include <stdlib.h>
#include <objc/objc.h>

#include "load-category-3.h"

@implementation TestClass2 (Category)
+ load
{
  printf ("[TestClass2(Category) +load]\n");

  /* Check that the corresponding class's +load was done.  */
  check_that_load_step_was_completed (1);

  complete_load_step (4);
}
@end

@implementation TestClass3 (Category)
+ load
{
  printf ("[TestClass3(Category) +load]\n");

  /* Check that the corresponding class's +load was done.  */
  check_that_load_step_was_completed (2);

  complete_load_step (5);
}
@end

@implementation TestClass1 (Category)
+ load
{
  printf ("[TestClass1(Category) +load]\n");

  /* Check that the corresponding class's +load was done.  */
  check_that_load_step_was_completed (0);

  complete_load_step (3);
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
