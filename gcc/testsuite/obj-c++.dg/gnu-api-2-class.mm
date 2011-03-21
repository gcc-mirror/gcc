/* Test the Modern GNU Objective-C Runtime API.

  This is test 'class', covering all functions starting with 'class'.  */

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

@interface MyOtherSubClass : MySubClass
@end

@implementation MyOtherSubClass
@end

@interface DifferentClass : MyRootClass
- (id) myClass;
- (id) self;
@end

@implementation DifferentClass
- (id) myClass { return object_getClass (self); }
- (id) self { return self; }
@end

@interface MySubClass (MySelf)
- (id) mySelf;
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

  std::cout << "Testing class_addIvar ()...\n";
  {
    Class new_class = objc_allocateClassPair (objc_getClass ("MySubClass"), "MySubSubClass", 0);

    if (new_class == Nil)
      abort ();
    
    if (! class_addIvar (new_class, "variable2_ivar", sizeof (id),
			 log_2_of (__alignof__ (id)), @encode (id)))
      abort ();

    if (! class_addIvar (new_class, "variable3_ivar", sizeof (unsigned char),
			 log_2_of (__alignof__ (unsigned char)), @encode (unsigned char)))
      abort ();

    if (! class_addIvar (new_class, "variable4_ivar", sizeof (unsigned long),
			 log_2_of (__alignof__ (unsigned long)), @encode (unsigned long)))
      abort ();

    objc_registerClassPair (new_class);    

    {
      MySubClass *o = [[objc_getClass ("MySubSubClass") alloc] init];
      Ivar variable2 = class_getInstanceVariable (objc_getClass ("MySubSubClass"), "variable2_ivar");
      Ivar variable3 = class_getInstanceVariable (objc_getClass ("MySubSubClass"), "variable3_ivar");
      Ivar variable4 = class_getInstanceVariable (objc_getClass ("MySubSubClass"), "variable4_ivar");

      if (variable2 == NULL  || variable3 == NULL  ||  variable4 == NULL)
	abort ();
      
      if (std::strcmp (ivar_getName (variable2), "variable2_ivar") != 0)
	abort ();

      if (std::strcmp (ivar_getName (variable3), "variable3_ivar") != 0)
	abort ();

      if (std::strcmp (ivar_getName (variable4), "variable4_ivar") != 0)
	abort ();

      {
	unsigned char *var3 = (unsigned char *)((char *)o + ivar_getOffset (variable3));
	unsigned long *var4 = (unsigned long *)((char *)o + ivar_getOffset (variable4));

	object_setIvar (o, variable2, new_class);
	*var3 = 230;
	*var4 = 89000L;

	if (object_getIvar (o, variable2) != new_class)
	  abort ();

	if (*var3 != 230)
	  abort ();

	if (*var4 != 89000L)
	  abort ();
      }
    }
  }

  std::cout << "Testing class_addMethod ()...\n";
  {
    Class new_class = objc_allocateClassPair (objc_getClass ("MyRootClass"), "MySubClass2", 0);
    Method method1 = class_getInstanceMethod (objc_getClass ("MySubClass"), @selector (setVariable:));
    Method method2 = class_getInstanceMethod (objc_getClass ("MySubClass"), @selector (variable));

    if (new_class == Nil)
      abort ();
    
    if (! class_addIvar (new_class, "variable_ivar", sizeof (id),
			 log_2_of (__alignof__ (id)), @encode (id)))
      abort ();

    if (! class_addMethod (new_class, @selector (setVariable:), method_getImplementation (method1),
			   method_getTypeEncoding (method1)))
      abort ();

    if (! class_addMethod (new_class, @selector (variable), method_getImplementation (method2),
			   method_getTypeEncoding (method2)))
      abort ();

    /* Test that if the method already exists in the class,
       class_addMethod() returns NO.  */
    if (class_addMethod (new_class, @selector (variable), method_getImplementation (method2),
			 method_getTypeEncoding (method2)))
      abort ();

    objc_registerClassPair (new_class);    

    /* Now, MySubClass2 is basically the same as MySubClass!  We'll
       use the variable and setVariable: methods on it.  */
    {
      MySubClass *o = (MySubClass *)[[objc_getClass ("MySubClass2") alloc] init];

      [o setVariable: o];

      if ([o variable] != o)
	abort ();
    }

    /* Now, try that if you take an existing class and try to add an
       already existing method, class_addMethod returns NO.  This is
       subtly different from before, when 'new_class' was still in
       construction.  Now it's a real class and the libobjc internals
       differ between the two cases.  */
    if (class_addMethod (new_class, @selector (variable), method_getImplementation (method2),
			 method_getTypeEncoding (method2)))
      abort ();
  }

  std::cout << "Testing class_addProtocol ()...\n";
  {
    if (!class_addProtocol (objc_getClass ("MySubClass"), @protocol (MySecondProtocol)))
      abort ();
    
    if (!class_conformsToProtocol (objc_getClass ("MySubClass"), @protocol (MyProtocol)))
      abort ();

    if (!class_conformsToProtocol (objc_getClass ("MySubClass"), @protocol (MySecondProtocol)))
      abort ();
  }

  std::cout << "Testing class_conformsToProtocol ()...\n";
  {
    if (class_conformsToProtocol (objc_getClass ("MyRootClass"), @protocol (MyProtocol)))
      abort ();

    if (!class_conformsToProtocol (objc_getClass ("MySubClass"), @protocol (MyProtocol)))
      abort ();

    /* Test that class_conformsToProtocol checks the class, but not
       superclasses.  */
    if (class_conformsToProtocol (objc_getClass ("MyOtherSubClass"), @protocol (MyProtocol)))
      abort ();
  }

  std::cout << "Testing class_copyIvarList ()...\n";
  {
    unsigned int count;
    Ivar * list = class_copyIvarList (objc_getClass ("MySubClass"), &count);

    if (count != 1)
      abort ();

    if (std::strcmp (ivar_getName (list[0]), "variable_ivar") != 0)
      abort ();
    
    if (list[1] != NULL)
      abort ();
  }

  std::cout << "Testing class_copyMethodList ()...\n";
  {
    unsigned int count;
    Method * list = class_copyMethodList (objc_getClass ("MySubClass"), &count);

    if (count != 2)
      abort ();
    
    if (! ((std::strcmp (sel_getName (method_getName (list[0])), "variable") == 0
	    && std::strcmp (sel_getName (method_getName (list[1])), "setVariable:") == 0)
	   || (std::strcmp (sel_getName (method_getName (list[0])), "setVariable:") == 0
	       && std::strcmp (sel_getName (method_getName (list[1])), "variable") == 0)))
      abort ();
    
    if (list[2] != NULL)
      abort ();
  }

  /* TODO: Test new ABI (when available).  */
  std::cout << "Testing class_copyPropertyList ()...\n";
  {
    unsigned int count;
    objc_property_t * list = class_copyPropertyList (objc_getClass ("MySubClass"), &count);

    if (count != 0  ||  list != NULL)
      abort ();
  }

  std::cout << "Testing class_copyProtocolList ()...\n";
  {
    unsigned int count;
    Protocol ** list = class_copyProtocolList (objc_getClass ("MySubClass"), &count);

    /* Remember that we added MySecondProtocol in the test above.  */
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

  std::cout << "Testing class_createInstance ()...\n";
  {
    MySubClass *object = [[MySubClass alloc] init];

    [object setVariable: object];
    if ([object variable] != object)
      abort ();
  }

  std::cout << "Testing class_getClassMethod ()...\n";
  {
    Method method = class_getClassMethod (objc_getClass ("MySubClass"),
					  @selector(alloc));

    if (method == NULL)
      abort ();

    if (std::strcmp (sel_getName (method_getName (method)), "alloc") != 0)
      abort ();

    if (class_getClassMethod (objc_getClass ("MySubClass"), 
			      @selector(variable)))
      abort ();
  }

  std::cout << "Testing class_getClassVariable ()...\n";
  {
    if (class_getClassVariable (objc_getClass ("MySubClass"), "variable_ivar"))
      abort ();
  }

  std::cout << "Testing class_getInstanceMethod ()...\n";
  {
    Method method = class_getInstanceMethod (objc_getClass ("MySubClass"), 
					     @selector(variable));

    if (method == NULL)
      abort ();

    if (std::strcmp (sel_getName (method_getName (method)), "variable") != 0)
      abort ();

    if (class_getInstanceMethod (objc_getClass ("MySubClass"), 
				 @selector(alloc)))
      abort ();
  }

  std::cout << "Testing class_getInstanceSize ()...\n";
  {
    if (class_getInstanceSize (objc_getClass ("MyRootClass")) != sizeof (struct objc_object))
      abort ();
  }

  std::cout << "Testing class_getInstanceVariable ()...\n";
  {
    Ivar variable = class_getInstanceVariable (objc_getClass ("MySubClass"), "variable_ivar");

    if (variable == NULL)
      abort ();

    if (std::strcmp (ivar_getName (variable), "variable_ivar") != 0)
      abort ();

    if (class_getInstanceVariable (objc_getClass ("MySubClass"), "variable_ivar_no"))
      abort ();
  }

  std::cout << "Testing class_getIvarLayout ()...\n";
  {
    if (class_getIvarLayout (objc_getClass ("MyRootClass")) != NULL)
      abort ();
  }

  std::cout << "Testing class_getMethodImplementation ()...\n";
  {
    MySubClass *object = [[MySubClass alloc] init];
    IMP imp = class_getMethodImplementation (objc_getClass ("MySubClass"), 
					     @selector(variable));

    if (imp == NULL)
      abort ();

    [object setVariable: object];

    if ((*imp)(object, @selector(variable)) != object)
      abort ();
  }

  /* This function does not exist with the GNU runtime.  */
  /* std::cout << "Testing class_getMethodImplementation_stret ()...\n"; */

  std::cout << "Testing class_getName ()...\n";
  {
    if (std::strcmp (class_getName (objc_getClass ("MyRootClass")),
		"MyRootClass") != 0)
      abort ();
  }

  /* TODO: Test new ABI (when available).  */
  std::cout << "Testing class_getProperty ()...\n";
  {
    if (class_getProperty (objc_getClass ("MyRootClass"), "property") != NULL)
      abort ();
  }

  std::cout << "Testing class_getSuperclass ()...\n";
  {
    MySubClass *object = [[MySubClass alloc] init];
    if (class_getSuperclass (object_getClass (object)) != objc_getClass ("MyRootClass"))
      abort ();
  }

  std::cout << "Testing class_getVersion ()...\n";
  {
    if (class_getVersion (objc_getClass ("MySubClass")) != 0)
      abort ();
  }

   std::cout << "Testing class_getWeakIvarLayout ()...\n";
  {
    if (class_getWeakIvarLayout (objc_getClass ("MyRootClass")) != NULL)
      abort ();
  }

  std::cout << "Testing class_isMetaClass ()...\n";
  {
    MySubClass *object = [[MySubClass alloc] init];
    if (class_isMetaClass (object_getClass (object)) 
	|| ! class_isMetaClass (object_getClass (object_getClass (object))))
      abort ();
  }

  std::cout << "Testing class_replaceMethod ()...\n";
  {
    Method new_method = class_getInstanceMethod (objc_getClass ("DifferentClass"),
						 @selector (myClass));
    Method old_method = class_getInstanceMethod (objc_getClass ("MySubClass"),
						 @selector (variable));
    const char *new_types = method_getTypeEncoding (new_method);
    IMP new_imp = method_getImplementation (new_method);
    const char *old_types = method_getTypeEncoding (old_method);
    IMP old_imp = class_replaceMethod (objc_getClass ("MySubClass"), @selector (variable),
				       method_getImplementation (new_method),
				       method_getTypeEncoding (new_method));
    MySubClass *o = [[MySubClass alloc] init];

    [o setVariable: o];

    /* Try the new method implementation.  */
    if ([o variable] != objc_getClass ("MySubClass"))
      abort ();

    /* Put the original method back.  */
    class_replaceMethod (objc_getClass ("MySubClass"), @selector (variable),
			 old_imp, old_types);

    /* Test it's back to what it was.  */
    if ([o variable] != o)
      abort ();    

    {
      DifferentClass *o = [[DifferentClass alloc] init];

      /* Finally, try adding a new method.  */
      class_replaceMethod (objc_getClass ("DifferentClass"), @selector (mySelf),
			   new_imp, new_types);
      
      if ([(MySubClass*)o mySelf] != objc_getClass ("DifferentClass"))
	abort ();
    }
  }

  std::cout << "Testing class_respondsToSelector ()...\n";
  {
    if (! class_respondsToSelector (objc_getClass ("MySubClass"), @selector(setVariable:)))
      abort ();

    if (class_respondsToSelector (objc_getClass ("MyRootClass"), @selector(setVariable:)))
      abort ();
  }

  /* This is not really implemented with the GNU runtime.  */
  /* std::cout << "Testing class_setIvarLayout ()...\n"; */

  std::cout << "Testing class_setVersion ()...\n";
  {
    class_setVersion (objc_getClass ("MySubClass"), 45);
    
    if (class_getVersion (objc_getClass ("MySubClass")) != 45)
      abort ();

    class_setVersion (objc_getClass ("MySubClass"), 46);

    if (class_getVersion (objc_getClass ("MySubClass")) != 46)
      abort ();
  }

  /* This is not really implemented with the GNU runtime.  */
  /* std::cout << "Testing class_setWeakIvarLayout ()...\n"; */

  return (0);
}
