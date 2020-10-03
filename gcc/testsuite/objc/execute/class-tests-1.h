/* Contributed by Nicola Pero on Tue Mar  6 23:05:53 CET 2001 */

#include <stdio.h>
#include <stdlib.h>
#include "../../objc-obj-c++-shared/runtime.h"

extern int strcmp(const char *, const char *);

/*
 * Standard Tests For Classes and Objects - abort upon failing; return
 * normally if all is well.
 */

/* Test that `class' is a Class */
static void test_is_class (Class class)
{
  if (class_isMetaClass (object_getClass (class)) == NO)
    {
      printf ("test_is_class failed\n");
      abort ();
    }
}

/* Test that the superclass of `class' is `superclass' */
static void test_superclass (Class class, Class superclass)
{
  if (class_getSuperclass (class) != superclass)
    {
      printf ("test_superclass failed\n");
      abort ();
    }
}

/* Test that the classname of `class' is `classname' */
static void test_class_name (Class class, const char *classname)
{
  if (strcmp (class_getName (class), classname))
    {
      printf ("test_class_name failed\n");
      abort ();
    }
}

/* Test that we can allocate instances of `class' */
static void test_allocate (Class class)
{
  /* The object we create is leaked but who cares, this is only a test */
  id object = class_createInstance (class, 0);

  if (object == nil)
    {
      printf ("test_allocate failed\n");
      abort ();
    }
}

/* Test that instances of `class' are instances and not classes */
static void test_instances (Class class)
{
  id object = class_createInstance (class, 0);

  if (class_isMetaClass (object_getClass (object)) == YES)
    {
      printf ("test_instances failed\n");
      abort ();
    }
}

/* Test that we can deallocate instances of `class' */
static void test_deallocate (Class class)
{
  id object = class_createInstance (class, 0);

  object_dispose (object);
}

/* Test that the object and the class agree on what the class is */
static void test_object_class (Class class)
{
  id object = class_createInstance (class, 0);

  if (object_getClass (object) != class)
    {
      printf ("test_object_class failed\n");
      abort ();
    }
}

/* 
 *  Runs all the tests in this file for the specified class 
 */
void test_class_with_superclass (const char *class_name, 
				 const char *superclass_name)
{
  Class class; 
  Class superclass; 

  /* class_name must be an existing class */
  class = objc_getClass (class_name);
  test_is_class (class);

  /* But superclass_name can be "", which means `Nil' */
  superclass = objc_getClass (superclass_name);  
  if (superclass != Nil)
    {
      test_is_class (superclass);
    }

  /* Now the tests */
  test_superclass (class, superclass);
  test_class_name (class, class_name);
  test_allocate (class);
  test_instances (class);
  test_deallocate (class);
  test_object_class (class);
}
