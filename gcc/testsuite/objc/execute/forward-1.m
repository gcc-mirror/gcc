/* See if -forward::/-performv:: is able to work. */

#include <stdio.h>
#include <stdlib.h>

#import "../../objc-obj-c++-shared/Object1.h"
#import "../../objc-obj-c++-shared/next-mapping.h"
#include <objc/objc-api.h>

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
