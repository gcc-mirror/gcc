/* Test out '@catch(id foo) {...}', which should catch
   all uncaught exceptions.  */
/* Developed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */
/* { dg-xfail-if "PR23616" { "*-*-*" } { "*" } { "" } } */
/* { dg-options "-fobjc-exceptions" } */

#include "../objc-obj-c++-shared/Object1.h"
#include <stdio.h>
#include <stdlib.h>

/* The following is not required in actual user code; we include it
   here to check that the compiler generates an internal definition of
   _setjmp that is consistent with what <setjmp.h> provides.  */
#include <setjmp.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@interface Frob: Object
@end

@implementation Frob: Object
@end

static Frob* _connection = nil;

//--------------------------------------------------------------------


void test (Object* sendPort)
{
  int cleanupPorts = 1;
  Frob* receivePort = nil;
	
  @try {
    printf ("receivePort = %p\n", receivePort);
    printf ("sendPort = %p\n", sendPort);
    printf ("cleanupPorts = %d\n", cleanupPorts);
    printf ("---\n");
		
    receivePort = (Frob *) -1;
    _connection = (Frob *) -1;
    printf ("receivePort = %p\n", receivePort);
    printf ("sendPort = %p\n", sendPort);
    printf ("cleanupPorts = %d\n", cleanupPorts);
    printf ("---\n");
		
    receivePort = nil;
    sendPort = nil;
    cleanupPorts = 0;
		
    printf ("receivePort = %p\n", receivePort);
    printf ("sendPort = %p\n", sendPort);
    printf ("cleanupPorts = %d\n", cleanupPorts);
    printf ("---\n");		
		
    @throw [Object new];
  }
  @catch(Frob *obj) {
    printf ("Exception caught by incorrect handler!\n");
    CHECK_IF(0);
  }
  @catch(id exc) {
    printf ("Exception caught by correct handler.\n");
    printf ("receivePort = %p (expected 0x0)\n", receivePort);
    printf ("sendPort = %p (expected 0x0)\n", sendPort);
    printf ("cleanupPorts = %d (expected 0)\n", cleanupPorts);
    printf ("---");
    CHECK_IF(!receivePort);
    CHECK_IF(!sendPort);
    CHECK_IF(!cleanupPorts);
  }
}

int main (void) {
  test((Object *)-1);
  return 0;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
