/* Test the Modern GNU Objective-C Runtime API.

   This is test 'resolve-method', covering +resolveClassMethod: and
   +resolveInstanceMethod:.  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>

/* For __objc_msg_forward2.  */
#include <objc/message.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
@end


/* A number of tests will try invoking methods that don't exist.  We
   want to record the fact, but not abort the program, so we supply
   our own fowarding implementation which will invoke the following
   function for any method that is not found.  */

/* Keep track of how many times a non-existing method was executed.  */
static int nonExistingMethodCount = 0;

/* Inspired by nil_method in libobjc.  */
id nonExisting_method (id receiver __attribute__ ((__unused__)),
		       SEL sel __attribute__ ((__unused__)))
{
  nonExistingMethodCount++;
  return nil;
}

/* Keep track of how many times the forwarding lookup was invoked.  */
static int forwardingCount = 0;

/* We install this forwarding hook to cause all failed method lookups
   to call our 'nonExisting_method' function.  */
IMP forward_everything_to_non_existing_method (id receiver __attribute__ ((__unused__)),
					       SEL sel __attribute__ ((__unused__)))
{
  forwardingCount++;
  return (IMP)nonExisting_method;
}


/* 'CountClass' is used to test that +resolveClassMethod: and
   +resolveInstanceMethod: are called when expected.  They do nothing
   other than recording that they are called.  */
@interface CountClass : MyRootClass
+ (BOOL) resolveClassMethod: (SEL)selector;
+ (BOOL) resolveInstanceMethod: (SEL)selector;
+ (void) existingClassMethod;
- (void) existingInstanceMethod;
@end

/* Count how many times the methods are called for class
   'CountClass'.  */
static int resolveClassMethodCount = 0;
static int resolveInstanceMethodCount = 0;

@implementation CountClass : MyRootClass
+ (BOOL) resolveClassMethod: (SEL)selector
{
  resolveClassMethodCount++;
  return NO;
}
+ (BOOL) resolveInstanceMethod: (SEL)selector
{
  resolveInstanceMethodCount++;
  return NO;
}
+ (void) existingClassMethod
{
  return;
}
- (void) existingInstanceMethod
{
  return;
}
@end

@protocol NonExistingStuff
+ (void) nonExistingClassMethod;
- (void) nonExistingInstanceMethod;
@end

/* Declare a category with some non existing methods, but don't
   actually implement them.  */
@interface CountClass (NonExistingStuff) <NonExistingStuff>
@end


/* 'SelfExtendingClass' is used to test that +resolveClassMethod: and
   +resolveInstanceMethod: can extend the class.  Any time they are
   called, they install the requested method, mapping it to the same
   implementation as 'countHits'.  */
@interface SelfExtendingClass : MyRootClass
+ (int) countHits;
+ (BOOL) resolveClassMethod: (SEL)selector;
+ (BOOL) resolveInstanceMethod: (SEL)selector;
@end

/* How many times the countHits method (or a clone) was called.  */
static int hitCount = 0;

@implementation SelfExtendingClass : MyRootClass
+ (int) countHits
{
  hitCount++;
  return hitCount;
}
+ (BOOL) resolveClassMethod: (SEL)selector
{
  /* Duplicate the 'countHits' method into the new method.  */
  Method method = class_getClassMethod (self, @selector (countHits));
  class_addMethod (object_getClass (self), selector,
		   method_getImplementation (method),
		   method_getTypeEncoding (method));
  resolveClassMethodCount++;
  return YES;
}
+ (BOOL) resolveInstanceMethod: (SEL)selector
{
  /* Duplicate the 'countHits' method into the new method.  */
  Method method = class_getClassMethod (self, @selector (countHits));
  class_addMethod (self, selector,
		   method_getImplementation (method),
		   method_getTypeEncoding (method));
  resolveInstanceMethodCount++;
  return YES;

}
@end

@protocol NonExistingStuff2
+ (int) nonExistingCountHitsMethod;
- (int) nonExistingCountHitsMethod;

+ (int) nonExistingCountHitsMethod2;
- (int) nonExistingCountHitsMethod2;

+ (int) nonExistingCountHitsMethod3;
- (int) nonExistingCountHitsMethod3;
@end

/* Declare a category with some non existing methods, but don't
   actually implement them.  */
@interface SelfExtendingClass (NonExistingStuff) <NonExistingStuff2>
@end


int main (int argc, void **args)
{
  /* Functions are tested in alphabetical order.  */

  /* Install our test forwarding hook.  */
  __objc_msg_forward2 = forward_everything_to_non_existing_method;

  printf ("Testing [+resolveClassMethod:]...\n");
  {
    Method m;
    IMP i;

    /** CountClass tests.  **/

    /* Call an existing method.  No +resolveClassMethod and no
       forwarding should be triggered.  */
    [CountClass existingClassMethod];

    if (resolveClassMethodCount != 0)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Call a non-existing method.  Both +resolveClassMethod and the
       forwarding should be triggered.  */
    [CountClass nonExistingClassMethod];

    if (resolveClassMethodCount != 1)
      abort ();

    if (forwardingCount != 1)
      abort ();

    if (nonExistingMethodCount != 1)
      abort ();

    /* Now try the same tests with class_getClassMethod(), which
       should trigger the resolve methods too, but not the
       forwarding.  */
    m = class_getClassMethod (objc_getClass ("CountClass"),
			      @selector (existingClassMethod));
    if (resolveClassMethodCount != 1)
      abort ();

    if (forwardingCount != 1)
      abort ();
    
    if (nonExistingMethodCount != 1)
      abort ();

    m = class_getClassMethod (objc_getClass ("CountClass"),
			      @selector (nonExistingClassMethod));
    if (resolveClassMethodCount != 2)
      abort ();

    if (forwardingCount != 1)
      abort ();
    
    if (nonExistingMethodCount != 1)
      abort ();

    /* Now try the same tests with class_getMethodImplementation(),
       which should trigger the resolve methods and the forwarding
       (but not execute the forwarding, obviously).  */
    i = class_getMethodImplementation (object_getClass (objc_getClass ("CountClass")),
				       @selector (existingClassMethod));
    if (resolveClassMethodCount != 2)
      abort ();

    if (forwardingCount != 1)
      abort ();
    
    if (nonExistingMethodCount != 1)
      abort ();

    i = class_getMethodImplementation (object_getClass (objc_getClass ("CountClass")),
				       @selector (nonExistingClassMethod));
    if (resolveClassMethodCount != 3)
      abort ();
    
    if (forwardingCount != 2)
      abort ();

    if (nonExistingMethodCount != 1)
      abort ();


    /* Reset the counters for the next test.  */
    resolveClassMethodCount = 0;
    forwardingCount = 0;
    nonExistingMethodCount = 0;


    /** SelfExtendingClass tests.  **/

    /* Try the direct countHits method first.  No resolving or
       forwarding should be triggered.  */
    if ([SelfExtendingClass countHits] != 1)
      abort ();

    if (resolveClassMethodCount != 0)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Now, try calling a non-existing count method; it should be
       installed and invoked.  */
    if ([SelfExtendingClass nonExistingCountHitsMethod] != 2)
      abort ();

    if (resolveClassMethodCount != 1)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Try it again.  The method has now been installed, so it should
       be used and work, but with no additional resolving
       involved.  */
    if ([SelfExtendingClass nonExistingCountHitsMethod] != 3)
      abort ();

    if (resolveClassMethodCount != 1)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();


    /* Now try the same tests with class_getClassMethod().  */
    m = class_getClassMethod (objc_getClass ("SelfExtendingClass"),
			      @selector (nonExistingCountHitsMethod2));
    if (resolveClassMethodCount != 2)
      abort ();

    if (forwardingCount != 0)
      abort ();
    
    if (nonExistingMethodCount != 0)
      abort ();

    /* Try it again.  The method has now been installed, so it should
       be used and work, but with no additional resolving
       involved.  */
    if ([SelfExtendingClass nonExistingCountHitsMethod2] != 4)
      abort ();

    if (resolveClassMethodCount != 2)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();


    /* Now try the same tests with class_getMethodImplementation().  */
    i = class_getMethodImplementation (object_getClass (objc_getClass ("SelfExtendingClass")),
				       @selector (nonExistingCountHitsMethod3));
    if (resolveClassMethodCount != 3)
      abort ();

    if (forwardingCount != 0)
      abort ();
    
    if (nonExistingMethodCount != 0)
      abort ();

    /* Try it again.  The method has now been installed, so it should
       be used and work, but with no additional resolving
       involved.  */
    if ([SelfExtendingClass nonExistingCountHitsMethod3] != 5)
      abort ();

    if (resolveClassMethodCount != 3)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();
  }

  /* Reset the counters for the next test.  */
  nonExistingMethodCount = 0;
  forwardingCount = 0;
  hitCount = 0;

  printf ("Testing [+resolveInstanceMethod:]...\n");
  {
    Method m;
    IMP i;
    CountClass *object = [[CountClass alloc] init];
    SelfExtendingClass *object2 = [[SelfExtendingClass alloc] init];

    /** CountClass tests.  **/

    /* Call an existing method.  No +resolveInstanceMethod and no
       forwarding should be triggered.  */
    [object existingInstanceMethod];

    if (resolveInstanceMethodCount != 0)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Call a non-existing method.  Both +resolveInstanceMethod and the
       forwarding should be triggered.  */
    [object nonExistingInstanceMethod];

    if (resolveInstanceMethodCount != 1)
      abort ();

    if (forwardingCount != 1)
      abort ();

    if (nonExistingMethodCount != 1)
      abort ();

    /* Now try the same tests with class_getInstanceMethod(), which
       should trigger the resolve methods too, but not the
       forwarding.  */
    m = class_getInstanceMethod (objc_getClass ("CountClass"),
				 @selector (existingInstanceMethod));
    
    if (resolveInstanceMethodCount != 1)
      abort ();

    if (forwardingCount != 1)
      abort ();
    
    if (nonExistingMethodCount != 1)
      abort ();

    m = class_getInstanceMethod (objc_getClass ("CountClass"),
				 @selector (nonExistingInstanceMethod));

    if (resolveInstanceMethodCount != 2)
      abort ();

    if (forwardingCount != 1)
      abort ();
    
    if (nonExistingMethodCount != 1)
      abort ();

    /* Now try the same tests with class_getMethodImplementation(),
       which should trigger the resolve methods and the
       forwarding.  */
    i = class_getMethodImplementation (objc_getClass ("CountClass"),
				       @selector (existingInstanceMethod));
    if (resolveInstanceMethodCount != 2)
      abort ();

    if (forwardingCount != 1)
      abort ();
    
    if (nonExistingMethodCount != 1)
      abort ();

    i = class_getMethodImplementation (objc_getClass ("CountClass"),
				       @selector (nonExistingInstanceMethod));
    if (resolveInstanceMethodCount != 3)
      abort ();
    
    if (forwardingCount != 2)
      abort ();

    if (nonExistingMethodCount != 1)
      abort ();

    /* Reset the counters for the next test.  */
    resolveInstanceMethodCount = 0;
    forwardingCount = 0;
    nonExistingMethodCount = 0;


    /** SelfExtendingClass tests.  **/

    /* Try the direct countHits method first.  No resolving or
       forwarding should be triggered.  */
    if ([SelfExtendingClass countHits] != 1)
      abort ();

    if (resolveInstanceMethodCount != 0)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Now, try calling a non-existing count method; it should be
       installed and invoked.  */
    if ([object2 nonExistingCountHitsMethod] != 2)
      abort ();

    if (resolveInstanceMethodCount != 1)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Try it again.  The method has now been installed, so it should
       be used and work, but with no additional resolving
       involved.  */
    if ([object2 nonExistingCountHitsMethod] != 3)
      abort ();

    if (resolveInstanceMethodCount != 1)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();

    /* Now try the same tests with class_getInstanceMethod().  */
    m = class_getInstanceMethod (objc_getClass ("SelfExtendingClass"),
				 @selector (nonExistingCountHitsMethod2));
    if (resolveInstanceMethodCount != 2)
      abort ();

    if (forwardingCount != 0)
      abort ();
    
    if (nonExistingMethodCount != 0)
      abort ();

    /* Try it again.  The method has now been installed, so it should
       be used and work, but with no additional resolving
       involved.  */
    if ([object2 nonExistingCountHitsMethod2] != 4)
      abort ();

    if (resolveInstanceMethodCount != 2)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();


    /* Now try the same tests with class_getMethodImplementation().  */
    i = class_getMethodImplementation (objc_getClass ("SelfExtendingClass"),
				       @selector (nonExistingCountHitsMethod3));
    if (resolveInstanceMethodCount != 3)
      abort ();
    
    if (forwardingCount != 0)
      abort ();
    
    if (nonExistingMethodCount != 0)
      abort ();

    /* Try it again.  The method has now been installed, so it should
       be used and work, but with no additional resolving
       involved.  */
    if ([object2 nonExistingCountHitsMethod3] != 5)
      abort ();

    if (resolveInstanceMethodCount != 3)
      abort ();

    if (forwardingCount != 0)
      abort ();

    if (nonExistingMethodCount != 0)
      abort ();
  }


  return 0;
}
