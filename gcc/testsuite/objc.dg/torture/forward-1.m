/* { dg-do run } */
/* See if -forward::/-performv:: is able to work. */
/* { dg-xfail-run-if "PR36610" { ! { { i?86-*-* x86_64-*-* } && ilp32 } } { "-fgnu-runtime" } { "" } } */
/* { dg-skip-if "Needs OBJC2 Implementation" { *-*-darwin* && { lp64 } } { "-fnext-runtime" } { "" } } */
/* There is no implementation of forward: in the NeXT m64 libobjc/Object
   neither have we implemented this in our extensions - so we have to skip it
   for now.  */

#include <stdio.h>
#include <stdlib.h>

#ifndef __NEXT_RUNTIME__
#  include <objc/objc-api.h>
#endif
#include <objc/Object.h>

#define VALUETOUSE 1234567890

id forwarder, receiver;

@interface Forwarder: Object
{
    id receiver;
}

-initWithReceiver:theReceiver;

@end

@interface Receiver:Object
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
#ifdef __NEXT_RUNTIME__
- forward: (SEL)theSel: (marg_list)theArgFrame
#else
-(retval_t) forward: (SEL)theSel: (arglist_t)theArgFrame
#endif
{
  /* If we have a reciever try to perform on that object */
    if (receiver)
        return [receiver performv: theSel: theArgFrame];
    return [self doesNotRecognize:theSel];
}
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
