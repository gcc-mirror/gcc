/* Contributed by Nicola Pero on Tue Mar  6 23:05:53 CET 2001 */
#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/Object.h>
#include <stdlib.h>

/*
 * Standard Tests For Classes and Objects - abort upon failing; return
 * normally if all is well.
 */

/* Test that `class' is a Class */
static void test_is_class (Class class)
{
  if (object_is_class (class) == NO)
    {
      printf ("test_is_class failed\n");
      abort ();
    }

  if (class_is_class (class) == NO)
    {
      printf ("test_is_class failed\n");
      abort ();
    }
}

/* Test that the superclass of `class' is `superclass' */
static void test_superclass (Class class, Class superclass)
{
  if (class_get_super_class (class) != superclass) 
    {
      printf ("test_superclass failed\n");
      abort ();
    }
}

/* Test that the classname of `class' is `classname' */
static void test_class_name (Class class, const char *classname)
{
  if (strcmp (class_get_class_name (class), classname))
    {
      printf ("test_class_name failed\n");
      abort ();
    }
}

/* Test that we can allocate instances of `class' */
static void test_allocate (Class class)
{
  /* The object we create is leaked but who cares, this is only a test */
  id object = class_create_instance (class);

  if (object == nil)
    {
      printf ("test_allocate failed\n");
      abort ();
    }
}

/* Test that instances of `class' are instances and not classes */
static void test_instances (Class class)
{
  id object = class_create_instance (class);

  if (object_is_class (object) == YES)
    {
      printf ("test_instances failed\n");
      abort ();
    }
}

/* Test that we can deallocate instances of `class' */
static void test_deallocate (Class class)
{
  id object = class_create_instance (class);

  object_dispose (object);
}

/* Test that the object and the class agree on what the class is */
static void test_object_class (Class class)
{
  id object = class_create_instance (class);

  if (object_get_class (object) != class)
    {
      printf ("test_object_class failed\n");
      abort ();
    }
}

/* Test that the object and the class agree on what the superclass is */
static void test_object_super_class (Class class)
{
  id object = class_create_instance (class);

  if (object_get_super_class (object) != class_get_super_class (class))
    {
      printf ("test_object_super_class failed\n");
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

  /* We need at least a method call before playing with the internals, 
     so that the runtime will call __objc_resolve_class_links () */
  [Object class];

  /* class_name must be an existing class */
  class = objc_lookup_class (class_name);
  test_is_class (class);

  /* But superclass_name can be "", which means `Nil' */
  superclass = objc_lookup_class (superclass_name);  
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
  test_object_super_class (class);
}
