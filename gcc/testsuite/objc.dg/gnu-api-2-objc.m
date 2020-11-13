/* Test the Modern GNU Objective-C Runtime API.

  This is test 'objc', covering all functions starting with 'objc'.  */

/* { dg-do run } */
/* Although this works with the NeXT runtime in a sub-set of cases, some
   versions of the runtime header pulls in a number of system protocols,
   which causes the objc_copyProtocolList test to fail (in addition to those
   systems that don't have the V2 APis).  XFAILing the run is not useful
   since it will XPASS on the sub-set that works.  */
/* { dg-skip-if "Incompatible" { *-*-darwin* } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
+ initialize;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
+ initialize { return self; }
@end

@protocol MyProtocol
- (id) variable;
@end

@protocol MySecondProtocol
- (id) setVariable: (id)value;
@end

@interface MySubClass : MyRootClass <MyProtocol>
{ id variable_ivar; }
- (void) setVariable: (id)value;
- (id) variable;
@end

@implementation MySubClass
- (void) setVariable: (id)value { variable_ivar = value; }
- (id) variable { return variable_ivar; }
@end

/* Hack to calculate the log2 of a byte alignment.  */
unsigned char
log_2_of (unsigned int x)
{
  unsigned char result = 0;

  /* We count how many times we need to divide by 2 before we reach 1.
     This algorithm is good enough for the small numbers (such as 8,
     16 or 64) that we have to deal with.  */
  while (x > 1)
    {
      x = x / 2;
      result++;
    }

  return result;
}

int main(int argc, void **args)
{
  /* Functions are tested in alphabetical order.  */

  printf ("Testing objc_allocateClassPair ()...\n");
  {
    Class new_root_class = objc_allocateClassPair (Nil, "MyNewRootClass", 0);
    Class new_class = objc_allocateClassPair (objc_getClass ("MyRootClass"), "MyNewSubClass", 0);

    /* A new root class would obviously need at least an 'isa'
       instance variable.  */
    class_addIvar (new_root_class, "isa", sizeof (Class), log_2_of (__alignof__ (Class)),
		   @encode (Class));

    objc_registerClassPair (new_root_class);
    objc_registerClassPair (new_class);

    if (strcmp (class_getName (new_class), "MyNewSubClass") != 0)
      abort ();

    if (class_getSuperclass (new_class) != objc_getClass ("MyRootClass"))
      abort ();

    if (strcmp (class_getName (new_root_class), "MyNewRootClass") != 0)
      abort ();

    if (class_getSuperclass (new_root_class) != Nil)
      abort ();

    {
      MySubClass *o = [[(Class)objc_getClass ("MyNewSubClass") alloc] init];
      
      if (object_getClass (o) != objc_getClass ("MyNewSubClass"))
	abort ();
    }
  }

  printf ("Testing objc_copyProtocolList ()...\n");
  {
    /* Make sure both our two protocols are known to the runtime.  */
    id my_protocol = @protocol (MyProtocol);
    id my_second_protocol = @protocol (MySecondProtocol);
    unsigned int count;
    Protocol ** list = objc_copyProtocolList (&count);

    if (count != 2)
      abort ();

    if (! ((strcmp (protocol_getName (list[0]), "MyProtocol") == 0
	    && strcmp (protocol_getName (list[1]), "MySecondProtocol") == 0)
	   || (strcmp (protocol_getName (list[0]), "MySecondProtocol") == 0
	       && strcmp (protocol_getName (list[1]), "MyProtocol") == 0)))
      abort ();
    
    if (list[2] != NULL)
      abort ();
  }

  printf ("Testing objc_disposeClassPair ()...\n");
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"), @selector (setVariable:));
    Class new_class = objc_allocateClassPair (objc_getClass ("MyRootClass"), "MyNewSubClass2", 0);

    if (new_class == Nil)
      abort ();

    /* Add a bit of everything to the class to exercise undoing all these changes.  */

    /* Instance variable.  */
    class_addIvar (new_class, "my_variable", sizeof (float), log_2_of (__alignof__ (float)), @encode (float));

    /* Instance method.  */
    class_addMethod (new_class, @selector (setVariable:), method_getImplementation (method),
		     method_getTypeEncoding (method));

    /* Class method.  */
    class_addMethod (object_getClass (new_class), @selector (setVariable:), method_getImplementation (method),
		     method_getTypeEncoding (method));

    /* Protocol.  */
    class_addProtocol (new_class, @protocol (MyProtocol));

    objc_disposeClassPair (new_class);
  }

  /* This function currently does not exist with the GNU runtime.  */
  /* printf ("Testing objc_duplicateClass ()...\n"); */

  /* TODO - Test it when implemented in the GNU Runtime */
  /*  printf ("Testing objc_getAssociatedObject ()...\n");  */

  printf ("Testing objc_getClass ()...\n");
  {
    if (strcmp (class_getName (objc_getClass ("MySubClass")),
		"MySubClass") != 0)
      abort ();
  }

  printf ("Testing objc_getClassList ()...\n");
  {
    Class *list;
    int i, count, other_count;
    count = objc_getClassList (NULL, 0);

    /* count most likely will be 5, (MyRootClass, MySubClass,
       Protocol, Object, NXConstantString).  */
    if (count < 3)
      abort ();
    
    list = malloc (sizeof (Class) * count);
    other_count = objc_getClassList (list, count);

    if (other_count != count)
      abort ();

    /* Spot-check: search for class 'MyRootClass' in the list.  */
    for (i = 0; i < count; i++)
      {
	if (strcmp (class_getName (list[i]), "MyRootClass") == 0)
	  break;
      }
    if (i == count)
      abort ();

    /* Spot-check: search for class 'MySubClass' in the list.  */
    for (i = 0; i < count; i++)
      {
	if (strcmp (class_getName (list[i]), "MySubClass") == 0)
	  break;
      }
    if (i == count)
      abort ();

    /* Spot-check: search for class 'Protocol' in the list.  */
    for (i = 0; i < count; i++)
      {
	if (strcmp (class_getName (list[i]), "Protocol") == 0)
	  break;
      }
    if (i == count)
      abort ();
  }

  /* This function does not exist with the GNU runtime.  */
  /* printf ("Testing objc_getFutureClass ()...\n"); */

  printf ("Testing objc_getMetaClass ()...\n");
  {
    if (! class_isMetaClass (objc_getMetaClass ("MyRootClass")))
      abort ();
  }

  printf ("Testing objc_getProtocol ()...\n");
  {
    if (! protocol_isEqual (objc_getProtocol ("MyProtocol"), @protocol (MyProtocol)))
      abort ();
  }

  printf ("Testing objc_getRequiredClass ()...\n");
  {
    if (strcmp (class_getName (objc_getRequiredClass ("MyRootClass")),
		"MyRootClass") != 0)
      abort ();
  }

  printf ("Testing objc_lookUpClass ()...\n");
  {
    if (strcmp (class_getName (objc_lookUpClass ("MyRootClass")),
		"MyRootClass") != 0)
      abort ();
  }

  /* This function does not exist with the GNU runtime.  */
  /* printf ("Testing objc_setFutureClass ()...\n"); */

  printf ("Testing objc_registerClassPair ()...\n");
  {
    Class new_class = objc_allocateClassPair (objc_getClass ("MySubClass"), "MySubSubClass", 0);

    class_addProtocol (new_class, @protocol (MySecondProtocol));
    
    objc_registerClassPair (new_class);
    
    if (strcmp (class_getName (new_class), "MySubSubClass") != 0)
      abort ();

    if (class_getSuperclass (new_class) != objc_getClass ("MySubClass"))
      abort ();

    if (! class_conformsToProtocol (new_class, @protocol (MySecondProtocol)))
      abort ();
  }

  /* TODO - Test it when implemented in the GNU Runtime */
  /*  printf ("Testing objc_removeAssociatedObjects ()...\n");  */

  /* TODO - Test it when implemented in the GNU Runtime */
  /*  printf ("Testing objc_setAssociatedObject ()...\n");  */

  return 0;
}
