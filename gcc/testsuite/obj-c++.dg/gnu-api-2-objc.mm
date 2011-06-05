/* Test the Modern GNU Objective-C Runtime API.

  This is test 'objc', covering all functions starting with 'objc'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>
#include <iostream>
#include <cstring>

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

int main ()
{
  /* Functions are tested in alphabetical order.  */

  std::cout << "Testing objc_allocateClassPair ()...\n";
  {
    Class new_root_class = objc_allocateClassPair (Nil, "MyNewRootClass", 0);
    Class new_class = objc_allocateClassPair (objc_getClass ("MyRootClass"), "MyNewSubClass", 0);

    /* A new root class would obviously need at least an 'isa'
       instance variable.  */
    class_addIvar (new_root_class, "isa", sizeof (Class), log_2_of (__alignof__ (Class)),
		   @encode (Class));

    objc_registerClassPair (new_root_class);
    objc_registerClassPair (new_class);

    if (std::strcmp (class_getName (new_class), "MyNewSubClass") != 0)
      abort ();

    if (class_getSuperclass (new_class) != objc_getClass ("MyRootClass"))
      abort ();

    if (std::strcmp (class_getName (new_root_class), "MyNewRootClass") != 0)
      abort ();

    if (class_getSuperclass (new_root_class) != Nil)
      abort ();

    {
      MySubClass *o = [[(Class)objc_getClass ("MyNewSubClass") alloc] init];
      
      if (object_getClass (o) != objc_getClass ("MyNewSubClass"))
	abort ();
    }
  }

  std::cout << "Testing objc_copyProtocolList ()...\n";
  {
    /* Make sure both our two protocols are known to the runtime.  */
    id my_protocol = @protocol (MyProtocol);
    id my_second_protocol = @protocol (MySecondProtocol);
    unsigned int count;
    Protocol ** list = objc_copyProtocolList (&count);

    if (count != 2)
      abort ();

    if (! ((std::strcmp (protocol_getName (list[0]), "MyProtocol") == 0
	    && std::strcmp (protocol_getName (list[1]), "MySecondProtocol") == 0)
	   || (std::strcmp (protocol_getName (list[0]), "MySecondProtocol") == 0
	       && std::strcmp (protocol_getName (list[1]), "MyProtocol") == 0)))
      abort ();
    
    if (list[2] != NULL)
      abort ();
  }

  std::cout << "Testing objc_disposeClassPair ()...\n";
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
  /* std::cout << "Testing objc_duplicateClass ()...\n"; */

  /* TODO - Test it when implemented in the GNU Runtime */
  /*  std::cout << "Testing objc_getAssociatedObject ()...\n";  */

  std::cout << "Testing objc_getClass ()...\n";
  {
    if (std::strcmp (class_getName (objc_getClass ("MySubClass")),
			"MySubClass") != 0)
      abort ();
  }

  std::cout << "Testing objc_getClassList ()...\n";
  {
    Class *list;
    int i, count, other_count;
    count = objc_getClassList (NULL, 0);

    /* count most likely will be 5, (MyRootClass, MySubClass,
       Protocol, Object, NXConstantString).  */
    if (count < 3)
      abort ();
    
    list = (Class *)(malloc (sizeof (Class) * count));
    other_count = objc_getClassList (list, count);

    if (other_count != count)
      abort ();

    /* Spot-check: search for class 'MyRootClass' in the list.  */
    for (i = 0; i < count; i++)
      {
	if (std::strcmp (class_getName (list[i]), "MyRootClass") == 0)
	  break;
      }
    if (i == count)
      abort ();

    /* Spot-check: search for class 'MySubClass' in the list.  */
    for (i = 0; i < count; i++)
      {
	if (std::strcmp (class_getName (list[i]), "MySubClass") == 0)
	  break;
      }
    if (i == count)
      abort ();

    /* Spot-check: search for class 'Protocol' in the list.  */
    for (i = 0; i < count; i++)
      {
	if (std::strcmp (class_getName (list[i]), "Protocol") == 0)
	  break;
      }
    if (i == count)
      abort ();
  }

  /* This function does not exist with the GNU runtime.  */
  /* std::cout << "Testing objc_getFutureClass ()...\n"; */

  std::cout << "Testing objc_getMetaClass ()...\n";
  {
    if (! class_isMetaClass (objc_getMetaClass ("MyRootClass")))
      abort ();
  }

  std::cout << "Testing objc_getProtocol ()...\n";
  {
    if (! protocol_isEqual (objc_getProtocol ("MyProtocol"), @protocol (MyProtocol)))
      abort ();
  }

  std::cout << "Testing objc_getRequiredClass ()...\n";
  {
    if (std::strcmp (class_getName (objc_getRequiredClass ("MyRootClass")),
			"MyRootClass") != 0)
      abort ();
  }

  std::cout << "Testing objc_lookUpClass ()...\n";
  {
    if (std::strcmp (class_getName (objc_lookUpClass ("MyRootClass")),
			"MyRootClass") != 0)
      abort ();
  }

  /* This function does not exist with the GNU runtime.  */
  /* std::cout << "Testing objc_setFutureClass ()...\n"; */

  std::cout << "Testing objc_registerClassPair ()...\n";
  {
    Class new_class = objc_allocateClassPair (objc_getClass ("MySubClass"), "MySubSubClass", 0);

    class_addProtocol (new_class, @protocol (MySecondProtocol));
    
    objc_registerClassPair (new_class);
    
    if (std::strcmp (class_getName (new_class), "MySubSubClass") != 0)
      abort ();

    if (class_getSuperclass (new_class) != objc_getClass ("MySubClass"))
      abort ();

    if (! class_conformsToProtocol (new_class, @protocol (MySecondProtocol)))
      abort ();
  }

  /* TODO - Test it when implemented in the GNU Runtime */
  /*  std::cout << "Testing objc_removeAssociatedObjects ()...\n";  */

  /* TODO - Test it when implemented in the GNU Runtime */
  /*  std::cout << "Testing objc_setAssociatedObject ()...\n";  */

  return (0);
}
