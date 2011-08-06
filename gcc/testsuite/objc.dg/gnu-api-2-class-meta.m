/* Test the Modern GNU Objective-C Runtime API.

  This is test 'class-meta', covering calling functions starting with
  'class' but using a meta class as argument.

  Functions that manipulate methods (adding, replacing methods)
  usually take a meta class argument to manipulate the class methods
  instead of the instance ones.  This is an important part of the API
  that needs testing.

  Functions that manipulate instances, instance variables, properties
  and protocols at the moment must take a normal class as argument;
  calling them with a meta class as argument is of no particular use
  and generally produces a behaviour that is undocumented and/or
  undefined (and this is true with all runtimes).  As in the future
  this behaviour may be defined or documented (for example, if class
  variables are implemented as instance variables of meta classes) we
  avoid testing it for compatibility with future runtimes.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

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

static id static_variable = nil;

@interface MySubClass : MyRootClass
+ (void) setVariable: (id)value;
+ (id) variable;
@end

@implementation MySubClass
+ (void) setVariable: (id)value { static_variable = value; }
+ (id) variable { return static_variable; }
@end

@interface DifferentClass : MyRootClass
+ (id) myClass;
@end

@implementation DifferentClass
+ (id) myClass { return self; }
@end

@interface MySubClass (MySelf)
+ (id) mySelf;
@end

int main(int argc, void **args)
{
  /* Functions are tested in alphabetical order.  */

  /* Calling class_addIvar() with a meta class is not documented and
     (currently) of no use.  */
  /* printf ("Testing class_addIvar ()...\n"); */
  
  printf ("Testing class_addMethod () on a meta class...\n");
  {
    /* Here we test adding methods to meta classes, ie, adding class methods.  */
    Class new_class = objc_allocateClassPair (objc_getClass ("MyRootClass"), "MySubClass2", 0);
    Method method1 = class_getInstanceMethod (objc_getMetaClass ("MySubClass"), @selector (setVariable:));
    Method method2 = class_getInstanceMethod (objc_getMetaClass ("MySubClass"), @selector (variable));

    if (new_class == Nil)
      abort ();
    
    if (! class_addMethod (object_getClass (new_class), @selector (setVariable:), method_getImplementation (method1),
			   method_getTypeEncoding (method1)))
      abort ();

    if (! class_addMethod (object_getClass (new_class), @selector (variable), method_getImplementation (method2),
			   method_getTypeEncoding (method2)))
      abort ();
    
    /* Test that if the method already exists in the class,
       class_addMethod() returns NO.  */
    if (class_addMethod (object_getClass (new_class), @selector (variable), method_getImplementation (method2),
			 method_getTypeEncoding (method2)))
      abort ();
    
    objc_registerClassPair (new_class);
    
    /* Now, MySubClass2 is basically the same as MySubClass!  We'll
       use the +variable and +setVariable: methods on it.  */
    {
      Class c = objc_getClass ("MySubClass2");
      id o = [[MyRootClass alloc] init];

      [c setVariable: o];
      
      if ([c variable] != o)
	abort ();
    }
    
    /* Now, try that if you take an existing class and try to add an
       already existing method, class_addMethod returns NO.  This is
       subtly different from before, when 'new_class' was still in
       construction.  Now it's a real class and the libobjc internals
       differ between the two cases.  */
    if (class_addMethod (object_getClass (new_class), @selector (variable), method_getImplementation (method2),
			 method_getTypeEncoding (method2)))
      abort ();
  }

  /* Calling class_addProtocol() on a meta class is not documented and
     (currently) of no use.  */
  /* printf ("Testing class_addProtocol () on a meta class...\n"); */

  /* Calling class_conformsToProtocol() on a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_conformsToProtocol () on a meta class...\n"); */
  
  /* Calling class_copyIvarList() on a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_copyIvarList () on a meta class...\n"); */

  printf ("Testing class_copyMethodList () on a meta class...\n");
  {
    /* Test that you can copy the method list of a meta class.  They
       are the class methods of the class.  */
    unsigned int count;
    Method * list = class_copyMethodList (objc_getMetaClass ("MySubClass"), &count);

    if (count != 2)
      abort ();
    
    if (! ((strcmp (sel_getName (method_getName (list[0])), "variable") == 0
	    && strcmp (sel_getName (method_getName (list[1])), "setVariable:") == 0)
	   || (strcmp (sel_getName (method_getName (list[0])), "setVariable:") == 0
	       && strcmp (sel_getName (method_getName (list[1])), "variable") == 0)))
      abort ();
    
    if (list[2] != NULL)
      abort ();
  }

  /* Calling class_copyPropertyList() on a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_copyPropertyList () on a meta class...\n"); */

  /* Calling class_copyProtocolList() on a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_copyProtocolList () on a meta class...\n"); */

  /* Calling class_createInstance() on a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_createInstance () on a meta class...\n"); */

  /* Calling class_getClassMethod () on a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_getClassMethod () on a meta class...\n"); */

  /* Calling class_getClassVariable () on a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_getClassVariable () on a meta class ...\n"); */

  printf ("Testing class_getInstanceMethod () on a meta class...\n");
  {
    /* The instance method of a meta class is the class method with
       the same name of the class. */
    Method method_1 = class_getInstanceMethod (objc_getMetaClass ("MySubClass"),
					       @selector(variable));
    Method method_2 = class_getClassMethod (objc_getClass ("MySubClass"),
					    @selector(variable));
    
    if (method_1 == NULL || method_2 == NULL)
      abort ();

    if (method_1 != method_2)
      abort ();

    if (strcmp (sel_getName (method_getName (method_1)), "variable") != 0)
      abort ();
  }

  /* Calling class_getInstanceSize() with a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_getInstanceSize () on a meta class...\n"); */

  /* Calling class_getInstanceVariable() with a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_getInstanceVariable () on a meta class...\n"); */

  /* Calling class_getIvarLayout() with a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_getIvarLayout () on a meta class...\n"); */

  printf ("Testing class_getMethodImplementation () on a meta class...\n");
  {
    /* Getting the method implementation with a meta class returns a
       class method.  */
    MySubClass *object = [[MySubClass alloc] init];
    IMP imp = class_getMethodImplementation (objc_getMetaClass ("MySubClass"), 
					     @selector(variable));

    if (imp == NULL)
      abort ();

    [MySubClass setVariable: object];

    if ((*imp)(objc_getClass ("MySubClass"), @selector(variable)) != object)
      abort ();
  }

  /* This function does not exist with the GNU runtime.  */
  /* printf ("Testing class_getMethodImplementation_stret () on a meta class...\n"); */

  printf ("Testing class_getName () on a meta class...\n");
  {
    /* Traditionally, a meta class has the same name as the class.  */
    if (strcmp (class_getName (objc_getMetaClass ("MyRootClass")),
		"MyRootClass") != 0)
      abort ();
  }

  /* Calling class_getProperty() with a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_getProperty ()...\n"); */

  printf ("Testing class_getSuperclass () on a meta class...\n");
  {
    /* The superclass of a meta class is the meta class of the superclass.  */
    if (class_getSuperclass (objc_getMetaClass ("MySubClass")) != objc_getMetaClass ("MyRootClass"))
      abort ();

    /* Test that it works on a newly created, but not registered, class.  */
    {
      Class new_class = objc_allocateClassPair (objc_getClass ("MyRootClass"), "MySubClass3", 0);

      if (class_getSuperclass (object_getClass (new_class)) != object_getClass (objc_getClass ("MyRootClass")))
	abort ();
    }
  }

  /* Calling class_getVersion() with a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_getVersion ()...\n"); */

  /* Calling class_getWeakIvarLayout() with a meta class is not
     documented and (currently) of no use.  */
  /* printf ("Testing class_getWeakIvarLayout () on a meta class...\n"); */

  /* class_isMetaClass() is already tested in gnu-api-2-class.m  */
  /* printf ("Testing class_isMetaClass ()...\n"); */

  printf ("Testing class_replaceMethod () on a meta class...\n");
  {
    /* We are going to replace the [MySubclass +variable] method with
       the [DifferentClass +myClass] one.  */
    Method new_method = class_getClassMethod (objc_getClass ("DifferentClass"),
					      @selector (myClass));
    Method old_method = class_getClassMethod (objc_getClass ("MySubClass"),
					      @selector (variable));
    const char *new_types = method_getTypeEncoding (new_method);
    IMP new_imp = method_getImplementation (new_method);
    const char *old_types = method_getTypeEncoding (old_method);
    IMP old_imp = class_replaceMethod (objc_getMetaClass ("MySubClass"), @selector (variable),
				       method_getImplementation (new_method),
				       method_getTypeEncoding (new_method));
    id o = [[MyRootClass alloc] init];

    [MySubClass setVariable: o];

    /* Try the new method implementation.  */
    if ([MySubClass variable] != objc_getClass ("MySubClass"))
      abort ();

    /* Put the original method back.  */
    class_replaceMethod (objc_getMetaClass ("MySubClass"), @selector (variable),
			 old_imp, old_types);

    /* Test it's back to what it was.  */
    if ([MySubClass variable] != o)
      abort ();

    {
      /* Finally, try adding a new method.  */
      class_replaceMethod (objc_getMetaClass ("DifferentClass"), @selector (mySelf),
			   new_imp, new_types);
      
      if ([(Class)objc_getClass ("DifferentClass") mySelf] != objc_getClass ("DifferentClass"))
	abort ();
    }
  }

  printf ("Testing class_respondsToSelector () on a meta class...\n");
  {
    /* A meta class responds to a selector if and only if the class
       responds to the corresponding class method.  */
    if (! class_respondsToSelector (objc_getMetaClass ("MySubClass"), @selector(setVariable:)))
      abort ();

    if (class_respondsToSelector (objc_getMetaClass ("MyRootClass"), @selector(setVariable:)))
      abort ();
  }

  /* This is not really implemented with the GNU runtime.  */
  /* printf ("Testing class_setIvarLayout () on a meta class...\n"); */

  /* Calling class_setVersion() with a meta class is not documented
     and (currently) of no use.  */
  /* printf ("Testing class_setVersion () on a meta class...\n"); */

  /* This is not really implemented with the GNU runtime.  */
  /* printf ("Testing class_setWeakIvarLayout () on a meta class...\n"); */

  return 0;
}
