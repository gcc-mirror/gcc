/* Interface for the Object class for Objective-C.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

GNU CC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files compiled
   with GCC to produce an executable, this does not cause the resulting
   executable to be covered by the GNU General Public License.  This
   exception does not however invalidate any other reasons why the
   executable file might be covered by the GNU General Public License. */

#ifndef __object_INCLUDE_GNU
#define __object_INCLUDE_GNU

#include <objc/objc.h>
#include <objc/typedstream.h>

/*
 * All classes are derived from Object.  As such,
 * this is the overhead tacked onto those objects.
 */
@interface Object
{
    Class	isa;	/* A pointer to the instance's class structure */
}

        /* Initializing classes and instances */
+ initialize;
- init;

        /* Creating, freeing, and copying instances */
+ new;
+ alloc;
- free;
- copy;
- shallowCopy;
- deepen;
- deepCopy;

        /* Identifying classes */
- (Class)class;
- (Class)superClass;
- (MetaClass)metaClass;
- (const char *)name;

        /* Identifying and comparing objects */
- self;
- (unsigned int)hash;
- (BOOL)isEqual:anObject;
- (int)compare:anotherObject;

        /* Testing object type */
- (BOOL)isMetaClass;
- (BOOL)isClass;
- (BOOL)isInstance;

        /* Testing inheritance relationships */
- (BOOL)isKindOf:(Class)aClassObject;
- (BOOL)isMemberOf:(Class)aClassObject;
- (BOOL)isKindOfClassNamed:(const char *)aClassName;
- (BOOL)isMemberOfClassNamed:(const char *)aClassName;

        /* Testing class functionality */
+ (BOOL)instancesRespondTo:(SEL)aSel;
- (BOOL)respondsTo:(SEL)aSel;

	/* Testing protocol conformance */
- (BOOL)conformsTo:(Protocol*)aProtocol;

        /* Introspection */
+ (IMP)instanceMethodFor:(SEL)aSel;
- (IMP)methodFor:(SEL)aSel;
+ (struct objc_method_description *)descriptionForInstanceMethod:(SEL)aSel;
- (struct objc_method_description *)descriptionForMethod:(SEL)aSel;

        /* Sending messages determined at run time */
- perform:(SEL)aSel;
- perform:(SEL)aSel with:anObject;
- perform:(SEL)aSel with:anObject1 with:anObject2;

        /* Forwarding */
- (retval_t)forward:(SEL)aSel :(arglist_t)argFrame;
- (retval_t)performv:(SEL)aSel :(arglist_t)argFrame;

        /* Posing */
+ poseAs:(Class)aClassObject;
- (Class)transmuteClassTo:(Class)aClassObject;

        /* Enforcing intentions */
- subclassResponsibility:(SEL)aSel;
- notImplemented:(SEL)aSel;
- shouldNotImplement:(SEL)aSel;

        /* Error handling */
- doesNotRecognize:(SEL)aSel;
- error:(const char *)aString, ...;

        /* Archiving */
+ (int)version;
+ setVersion:(int)aVersion;
+ (int)streamVersion: (TypedStream*)aStream;

- read: (TypedStream*)aStream;
- write: (TypedStream*)aStream;
- awake;

@end

#endif
