/** Declaration of extension methods for base additions

   Copyright (C) 2003-2010 Free Software Foundation, Inc.

   Written by:  Richard Frith-Macdonald <rfm@gnu.org>
   and:         Adam Fedor <fedor@gnu.org>

   This file is part of the GNUstep Base Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02111 USA.

*/

#ifndef	INCLUDED_NSObject_GNUstepBase_h
#define	INCLUDED_NSObject_GNUstepBase_h

#import "GSVersionMacros.h"
#import "../Foundation/NSObject.h"

#if	defined(__cplusplus)
extern "C" {
#endif

#if	OS_API_VERSION(GS_API_NONE,GS_API_LATEST)

@class  NSHashTable;

@interface NSObject (GNUstepBase)

/**
  WARNING: The -compare: method for NSObject is deprecated
           due to subclasses declaring the same selector with 
	   conflicting signatures.
           Comparision of arbitrary objects is not just meaningless
           but also dangerous as most concrete implementations
           expect comparable objects as arguments often accessing
	   instance variables directly.
	   This method will be removed in a future release.
*/
- (NSComparisonResult) compare: (id)anObject;

/** For backward compatibility only ... use class_isMetaClass() on the
 * class of the receiver instead.
 */
- (BOOL) isInstance;

/** DEPRECATED ... do not use.
 * Transmutes the receiver into an immutable version of the same object
 * and returns the result.<br />
 * If the receiver is not a mutable object or cannot be simply transmuted,
 * then this method either returns the receiver unchanged or,
 * if the force flag is set to YES, returns an autoreleased copy of the
 * receiver.<br />
 * Mutable classes should override this default implementation.<br />
 * This method is used in methods which are declared to return immutable
 * objects (eg. an NSArray), but which create and build mutable ones
 * internally.
 */
- (id) makeImmutableCopyOnFail: (BOOL)force;

/** Transmutes the receiver into an immutable version of the same object.
 * Returns YES if the receiver has become immutable, NO otherwise.<br />
 * The default implementation returns NO.<br />
 * Mutable classes which have an immutable counterpart they can efficiently
 * change into, should override to transmute themselves and return YES.<br />
 * Immutable classes should override this to simply return YES with no
 * further action.<br />
 * This method is used in methods which are declared to return immutable
 * objects (eg. an NSArray), but which create and build mutable ones
 * internally.
 */
- (BOOL) makeImmutable;

/**
 * Message sent when an implementation wants to explicitly exclude a method
 * (but cannot due to compiler constraint), and wants to make sure it is not
 * called by mistake.  Default implementation raises an exception at runtime.
 */
- (id) notImplemented: (SEL)aSel GS_NORETURN_METHOD;

/**
 * Message sent when an implementation wants to explicitly require a subclass
 * to implement a method (but cannot at compile time since there is no
 * <code>abstract</code> keyword in Objective-C).  Default implementation
 * raises an exception at runtime to alert developer that he/she forgot to
 * override a method.
 */
- (id) subclassResponsibility: (SEL)aSel GS_NORETURN_METHOD;

/**
 * Message sent when an implementation wants to explicitly exclude a method
 * (but cannot due to compiler constraint) and forbid that subclasses
 * implement it.  Default implementation raises an exception at runtime.  If a
 * subclass <em>does</em> implement this method, however, the superclass's
 * implementation will not be called, so this is not a perfect mechanism.
 */
- (id) shouldNotImplement: (SEL)aSel GS_NORETURN_METHOD;

@end

/** This is an informal protocol ... classes may implement the method to
 * report how much memory is used by the instance and any objects it acts
 * as a container for.
 */
@interface      NSObject(MemoryFootprint)
/* This method returns the memory usage of the receiver, excluding any
 * objects already present in the exclude table.<br />
 * The argument is a hash table configured to hold non-retained pointer
 * objects and is used to inform the receiver that its size should not
 * be counted again if it's already in the table.<br />
 * The NSObject implementation returns zero if the receiver is in the
 * table, but otherwise adds itself to the table and returns its memory
 * footprint (the sum of all of its instance variables, but not any
 * memory pointed to by those variables).<br />
 * Subclasses should override this method by calling the superclass
 * implementation, and either return the result (if it was zero) or
 * return that value plus the sizes of any memory owned by the receiver
 * (eg found by calling the same method on objects pointed to by the
 * receiver's instance variables).
 */
- (NSUInteger) sizeInBytesExcluding: (NSHashTable*)exclude;
@end

/** This is an informal protocol ... classes may implement the method and
 * register themselves to have it called on process exit.
 */
@interface NSObject(GSAtExit)
/** This method is called on exit for any class which implements it and which
 * has called +registerAtExit to register it to be called.<br />
 * The order in which methods for different classes is called is the reverse
 * of the order in which the classes were registered, but it's best to assume
 * the method can not depend on any other class being in a usable state
 * at the point when the method is called (rather like +load).<br />
 * Typical use would be to release memory occupied by class data structures
 * so that memory usage analysis software will not think the memory has
 * been leaked.
 */
+ (void) atExit;
@end

/** Category for methods handling leaked memory cleanup on exit of process
 * (for use when debugging memory leaks).<br />
 * You enable this by calling the +setShouldCleanUp: method (done implicitly
 * by gnustep-base if the GNUSTEP_SHOULD_CLEAN_UP environment variable is
 * set to YES).<br />
 * Your class then has two options for performing cleanup when the process
 * ends:
 * <p>1. Use the +leak: method to register objects which are simply to be 
 * retained until the process ends, and then either ignored or released
 * depending on the cleanup setting in force.  This mechanism is simple
 * and should be sufficient for many classes.
 * </p>
 * <p>2. Implement a +atExit method to be run when the process ends and,
 * within your +initialize implementation, call +shouldCleanUp to determine
 * whether cleanup should be done, and if it returns YES then call
 * +registerAtExit to have your +atExit method called when the process
 * terminates.
 * </p>
 * <p>The order in which 'leaked' objects are released and +atExit methods
 * are called on process exist is the reverse of the order in which they
 * werse set up suing this API.
 * </p>
 */
@interface NSObject(GSCleanup)


/** This method simply retains its argument so that it will never be
 * deallocated during normal operation, but keeps track of it so that
 * it is released during process exit if cleanup is enabled.<br />
 * Returns its argument.
 */
+ (id) NS_RETURNS_RETAINED leak: (id)anObject;

/** This method retains the object at *anAddress so that it will never be
 * deallocated during normal operation, but keeps track of the address
 * so that the object is released and the address is zeroed during process
 * exit if cleanup is enabled.<br />
 * Returns the object at *anAddress.
 */
+ (id) NS_RETURNS_RETAINED leakAt: (id*)anAddress;

/** Sets the receiver to have its +atExit method called at the point when
 * the process terminates.<br />
 * Returns YES on success and NO on failure (if the class does not implement
 * the method or if it is already registered to call it).<br />
 * Implemented as a call to +registerAtExit: with the selector for the +atExit
 * method as its argument.
 */
+ (BOOL) registerAtExit;

/** Sets the receiver to have the specified  method called at the point when
 * the process terminates.<br />
 * Returns YES on success and NO on failure (if the class does not implement
 * the method ir if it is already registered to call it).
 */
+ (BOOL) registerAtExit: (SEL)aSelector;

/** Specifies the default cleanup behavior on process exit ... the value
 * returned by the NSObject implementation of the +shouldClanUp method.<br />
 * Calling this method with a YES argument implicitly calls the +enableAtExit
 * method as well.<br />
 * The GNUstep Base library calls this method with the value obtained from
 * the GNUSTEP_SHOULD_CLEAN_UP environment variable when NSObject is
 * initialised.
 */
+ (void) setShouldCleanUp: (BOOL)aFlag;

/** Returns a flag indicating whether the receiver should clean up
 * its data structures etc at process exit.<br />
 * The NSObject implementation returns the value set by the +setShouldCleanUp:
 * method but subclasses may override this.
 */
+ (BOOL) shouldCleanUp;

@end

/* Macro to take an autoreleased object and either make it immutable or
 * create an autoreleased copy of the original.
 */
#define GS_IMMUTABLE(O) ([O makeImmutable] == YES ? O : AUTORELEASE([O copy]))

#endif	/* OS_API_VERSION */

#if	defined(__cplusplus)
}
#endif

#endif	/* INCLUDED_NSObject_GNUstepBase_h */

