/* Object definition taken from <objc/Object.h>
 */
#ifndef _OBJC_OBJECT1_H_
#define _OBJC_OBJECT1_H_

#undef DO_NEXT_M64_OBJECT_IMPLEMENTATION

#ifndef __NEXT_RUNTIME__
#  include <objc/Object.h>
#else
#  include "next-abi.h"
#  ifndef NEXT_OBJC_USE_NEW_INTERFACE
/* We are on a next system, or version, that is happy to compile V0 ABI */
#    include <objc/Object.h>
#  else
#    if (NEXT_OBJC_ABI_VERSION==0)
/* We are on a system that has V0 ABI implementation in libobjc.dylib.
 * However, we need to use the new accessors and pretend that the 
 * structures are opaque to avoid 'deprecated' warnings
 */
#      include <objc/Object.h>
#    else
/* We are on a system that includes a V2 ABI Object in libobjc.dylib.
*/
#      ifdef __OBJC2__
/* ... and we have a V2 ABI compiler ..  */
#        include <objc/Object.h>
#      else
/* We can't access the Object definition in libobjc.dylib because
 * we can't yet generate OBJC2 code.
 *
 * So we'll roll our own Object - purely for the sake of compile
 * checks - the code is unlikely to run...
*/
#        ifndef _OBJC_OBJECT_H_
#        define _OBJC_OBJECT_H_

#include <stdarg.h>
#import <objc/objc-runtime.h>

/* This is a cut-down Object with only the methods currently required
   by the testsuite declared.
   
   For those executables that require an implementation (to link) this
   can be provided in a given test by placing:
   #include "path/to/objc-c++shared/Object1-implementation.h"
   at the end of the source for the test.
*/

@interface Object 
{
	Class isa; /* A pointer to the instance's class structure */
}
+ initialize;
- init;

+ new;
+ free;
- free;
+ alloc;
//- copy;
//+ allocFromZone:(void *)zone;
//- copyFromZone:(void *)zone;
//- (void *)zone;

+ class;
//+ superclass;
//+ (const char *) name;
- class;
- superclass;
- (const char *) name;

//- self;
//- (unsigned int) hash;
//-(BOOL) isEqual:anObject;

/* Testing inheritance relationships */

//- (BOOL) isKindOf: aClassObject;
//- (BOOL) isMemberOf: aClassObject;
//- (BOOL) isKindOfClassNamed: (const char *)aClassName;
//- (BOOL) isMemberOfClassNamed: (const char *)aClassName;

/* Testing class functionality */

//+ (BOOL) instancesRespondTo:(SEL)aSelector;
//- (BOOL) respondsTo:(SEL)aSelector;

/* Testing protocol conformance */

- (BOOL) conformsTo: (Protocol *)aProtocolObject;
//+ (BOOL) conformsTo: (Protocol *)aProtocolObject;

/* Obtaining method descriptors from protocols */

//- (struct objc_method_description *) descriptionForMethod:(SEL)aSel;
//+ (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel;

/* Obtaining method handles */

//- (IMP) methodFor:(SEL)aSelector;
//+ (IMP) instanceMethodFor:(SEL)aSelector;

/* Sending messages determined at run time */

//- perform:(SEL)aSelector;
//- perform:(SEL)aSelector with:anObject;
//- perform:(SEL)aSelector with:object1 with:object2;

/* Posing */

//+ poseAs: aClassObject;

/* Enforcing intentions */
 
//- subclassResponsibility:(SEL)aSelector;
//- notImplemented:(SEL)aSelector;

/* Error handling */

//- doesNotRecognize:(SEL)aSelector;
//- error:(const char *)aString, ...;

/* Debugging */

//- (void) printForDebugger:(void *)stream;

/* Archiving */

//- awake;
//- write:(void *)stream;
//- read:(void *)stream;
//+ (int) version;
//+ setVersion: (int) aVersion;

/* Forwarding */

//- forward: (SEL)sel : (marg_list)args;
//- performv: (SEL)sel : (marg_list)args;

@end

#define DO_NEXT_M64_OBJECT_IMPLEMENTATION

#        endif /* _OBJC_OBJECT_H_ */
#      endif /* __OBJC2__ */
#      endif /* ABI=0 */
#    endif /* NEXT_OBJC_USE_NEW_INTERFACE */
#   endif /* __NEXT_RUNTIME__ */
#endif /* _OBJC_OBJECT1_H_ */
