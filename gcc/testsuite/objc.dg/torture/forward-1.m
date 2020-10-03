/* { dg-do run } */
/* See if -forward:: is able to work. */
/* { dg-skip-if "Needs OBJC2 Implementation" { *-*-darwin8* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-options "-Wl,-framework,Foundation" { target *-*-darwin* } } */
#include <stdio.h>
#include <stdlib.h>

/* Versions of the runtime after 10.13 no longer support the original
   'forward:' mechanism, so we make a stripped down representation of
   NSInvocation and need to link with -framework Foundation.  */
#if __NEXT_RUNTIME__
@class  NSInvocation, NSMethodSignature;
# include "../../objc-obj-c++-shared/F-NSObject.h"
@interface NSInvocation : NSObject
+ (NSInvocation *)invocationWithMethodSignature:(NSMethodSignature *)sig;
@property SEL selector;
- (void)invoke;
- (void)invokeWithTarget:(id)target;
@end
# define OBJECT NSObject
#else
#include "../../objc-obj-c++-shared/TestsuiteObject.m"
#define OBJECT TestsuiteObject
#endif

#define VALUETOUSE 1234567890

id forwarder, receiver;

@interface Forwarder : OBJECT
{
    id receiver;
}

-initWithReceiver:theReceiver;
#if __NEXT_RUNTIME__
- (void)forwardInvocation:(NSInvocation *)anInvocation;
- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSelector;
#endif

@end

@interface Receiver : OBJECT
{
    int foo;
}
-display;
-initWithFoo:(int)theFoo;
@end

@implementation Receiver

-initWithFoo: (int)theFoo
{
    foo = theFoo;
    return self;
}

-display
{
  printf ("Executing display\n");
    /* Check to see if we are really the reciever. */
    if (self != receiver)
        abort ();
    /* And the value of foo is set correctly. */
    if (foo != VALUETOUSE)
      abort ();
    return self;
}

@end

@implementation Forwarder
-initWithReceiver: theReceiver
{
    [super init];
    receiver = theReceiver;
    return self;
}

#if __NEXT_RUNTIME__
- (NSMethodSignature *)methodSignatureForSelector:(SEL)aSel
{
 return [receiver methodSignatureForSelector:aSel];
}
- (void)forwardInvocation:(NSInvocation *)anInvocation
{
    if ([receiver respondsToSelector:[anInvocation selector]]) {
        [anInvocation invokeWithTarget:receiver];
    }
    else {
    }
}
#else
-(void *) forward:(SEL)theSel : (void *)theArgFrame
{
  /* If we have a reciever try to perform on that object */
    if (receiver)
      {
	/* Simple forward that works for methods with no
	   arguments.  */
	typedef id (*method_with_no_args) (id receiver, SEL _cmd);
	Method method = class_getInstanceMethod (object_getClass (receiver),
						 theSel);
	method_with_no_args imp = (method_with_no_args)(method_getImplementation
							(method));
	return (*imp)(receiver, theSel);
      }

    /* Normally you'd emit an error here.  */
    printf ("Unrecognized selector\n");
    return NULL;
}
#endif

@end
int main()
{
    /* Init the reciever. */
    receiver = [[Receiver alloc] initWithFoo: VALUETOUSE];
    /* Init the fowarder. */
    forwarder = [[Forwarder alloc] initWithReceiver: receiver];
    /* Call display on the forwarder which in turns calls display on
       the reciever. */
    [forwarder display];
    exit(0);
}
