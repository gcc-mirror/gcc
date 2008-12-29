/* See if -forward::/-performv:: is able to work. */

#include <stdio.h>
#include <stdlib.h>
#include <objc/Object.h>
#include <objc/objc-api.h>

#define VALUETOUSE 1234567890

#ifdef __NEXT_RUNTIME__
/* Does not run with the next runtime. */
int main(void)
{
  return 0;
}

#else

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
-(retval_t) forward: (SEL)theSel: (arglist_t)theArgFrame
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

#endif
