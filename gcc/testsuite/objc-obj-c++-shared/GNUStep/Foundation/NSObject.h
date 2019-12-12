/**Interface for NSObject for GNUStep
   Copyright (C) 1995, 1996, 1998 Free Software Foundation, Inc.

   Written by:  Andrew Kachites McCallum <mccallum@gnu.ai.mit.edu>
   Date: 1995
   
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

   AutogsdocSource: NSObject.m
   */ 

#ifndef __NSObject_h_GNUSTEP_BASE_INCLUDE
#define __NSObject_h_GNUSTEP_BASE_INCLUDE

#import	"NSObjCRuntime.h"
#import <objc/objc.h>
#import	"NSZone.h"

#ifdef	GS_WITH_GC
#undef  GS_WITH_GC
#endif
#define	GS_WITH_GC	0

#import	"../GNUstepBase/GNUstep.h"

#if	defined(__cplusplus)
extern "C" {
#endif

@class NSArchiver;
@class NSArray;
@class NSCoder;
@class NSDictionary;
@class NSPortCoder;
@class NSMethodSignature;
@class NSMutableString;
@class NSRecursiveLock;
@class NSString;
@class NSInvocation;
@class Protocol;

/**
 * The NSObject protocol describes a minimal set of methods that all
 * objects are expected to support.  You should be able to send any
 * of the messages listed in this protocol to an object, and be safe
 * in assuming that the receiver can handle it.
 */
@protocol NSObject
/**
 * Returns the class of the receiver.  If the receiver is a proxy, then this
 * may return the class of the proxy target.  Use -isProxy to determine whether
 * the receiver is a proxy.  If you wish to find the real class of the
 * receiver, ignoring proxies, then use object_getClass().  
 */
- (Class) class;
/**
 * Returns the superclass of receiver's class.  If the receiver is a proxy,
 * then this may return the class of the proxy target.  Use -isProxy to
 * determine whether the receiver is a proxy.  If you wish to find the real
 * superclass of the receiver's class, ignoring proxies, then use
 * class_getSuperclass(object_getClass()).
 */
- (Class) superclass;
/**
 * Returns whether the receiver is equal to the argument.  Defining equality is
 * complex, so be careful when implementing this method.  Collections such as
 * NSSet depend on the behaviour of this method.  In particular, this method
 * must be commutative, so for any objects a and b:
 *
 * [a isEqual: b] == [b isEqual: a]
 *
 * This means that you must be very careful when returning YES if the argument
 * is of another class.  For example, if you define a number class that returns
 * YES if the argument is a string representation of the number, then this will
 * break because the string will not recognise your object as being equal to
 * itself.
 *
 * If two objects are equal, then they must have the same hash value, however
 * equal hash values do not imply equality.
 */
- (BOOL) isEqual: (id)anObject;
/**
 * Returns YES if the receiver is an instance of the class, an instance of the
 * subclass, or (in the case of proxies), an instance of something that can be
 * treated as an instance of the class.
 */
- (BOOL) isKindOfClass: (Class)aClass;
/**
 * Returns YES if the receiver is an instance of the class or (in the case of
 * proxies), an instance of something that can be treated as an instance of the
 * class.
 *
 * Calling this method is rarely the correct thing to do.  In most cases, a
 * subclass can be substituted for a superclass, so you should never need to
 * check that an object is really an instance of a specific class and not a
 * subclass.  
 */
- (BOOL) isMemberOfClass: (Class)aClass;
/**
 * Returns YES if the receiver is a proxy, NO otherwise.  The default
 * implementation of this method in NSObject returns NO, while the
 * implementation in NSProxy returns YES.
 */
- (BOOL) isProxy;
/**
 * Returns a hash value for the object.  All objects that are equal *MUST*
 * return the same hash value.  For efficient storage in sets, or as keys in
 * dictionaries, different objects should return hashes spread evenly over the
 * range of an integer.
 *
 * An object may not return different values from this method after being
 * stored in a collection.  This typically means that ether the hash value must
 * be constant after the object's creation, or that the object may not be
 * modified while stored in an unordered collection.
 */
- (NSUInteger) hash;
/**
 * Returns the receiver.  In a proxy, this may (but is not required to) return
 * the proxied object.
 */
- (id) self;
/**
 * Performs the specified selector.  The selector must correspond to a method
 * that takes no arguments.
 */
- (id) performSelector: (SEL)aSelector;
/**
 * Performs the specified selector, with the object as the argument.  This
 * method does not perform any automatic unboxing, so the selector must
 * correspond to a method that takes one object argument.
 */
- (id) performSelector: (SEL)aSelector
	    withObject: (id)anObject;
/**
 * Performs the specified selector, with the objects as the arguments.  This
 * method does not perform any automatic unboxing, so the selector must
 * correspond to a method that takes two object arguments.
 */
- (id) performSelector: (SEL)aSelector
	    withObject: (id)object1
	    withObject: (id)object2;
/**
 * Returns YES if the object can respond to messages with the specified
 * selector.  The default implementation in NSObject returns YES if the
 * receiver has a method corresponding to the method, but other classes may
 * return YES if they can respond to a selector using one of the various
 * forwarding mechanisms.
 */
- (BOOL) respondsToSelector: (SEL)aSelector;
/**
 * Returns YES if the receiver conforms to the specified protocol.
 */
- (BOOL) conformsToProtocol: (Protocol*)aProtocol;
/**
 * Increments the reference count of the object and returns the receiver.  In
 * garbage collected mode, this method does nothing.  In automated reference
 * counting mode, you may neither implement this method nor call it directly.
 */
- (id) retain NS_AUTOMATED_REFCOUNT_UNAVAILABLE;
/**
 * Decrements the reference count of the object and destroys if it there are no
 * remaining references.  In garbage collected mode, this method does nothing.
 * In automated reference counting mode, you may neither implement this method
 * nor call it directly.
 */
- (oneway void) release NS_AUTOMATED_REFCOUNT_UNAVAILABLE;
/**
 * Performs a deferred -release operation.  The object's reference count is
 * decremented at the end of the scope of the current autorelease pool,
 * identified either by a -drain message sent to the current NSAutoreleasePool
 * instance, or in more recent versions of Objective-C by the end of an
 * @autorelease_pool scope.
 *
 * In garbage collected mode, this method does nothing.  In automated reference
 * counting mode, you may neither implement this method nor call it directly.
 */
- (id) autorelease NS_AUTOMATED_REFCOUNT_UNAVAILABLE;
/**
 * Returns the current retain count of an object.  This does not include the
 * result of any pending autorelease operations.
 *
 * Code that relies on this method returning a sane value is broken.  For
 * singletons, it may return NSUIntegerMax.  Even when it is tracking a retain
 * count, it will not include on-stack pointers in manual retain/release mode,
 * pointers marked as __unsafe_unretain or __weak in ARC mode, or pending
 * autorelease operations.  Its value is therefore largely meaningless.  It can
 * occasionally be useful for debugging.
 */
- (NSUInteger) retainCount NS_AUTOMATED_REFCOUNT_UNAVAILABLE;
/**
 * Returns the description of the object.  This is used by the %@ format
 * specifier in strings.
 */
- (NSString*) description;
/**
 * Returns the zone of the object.
 */
- (NSZone*) zone NS_AUTOMATED_REFCOUNT_UNAVAILABLE;
@end

/**
 * This protocol must be adopted by any class wishing to support copying -
 * ie where instances of the class should be able to create new instances
 * which are copies of the original and, where a class has mutable and
 * immutable versions, where the copies are immutable.
 */
@protocol NSCopying
/**
 * Called by [NSObject-copy] passing NSDefaultMallocZone() as zone.<br />
 * This method returns a copy of the receiver and, where the receiver is a
 * mutable variant of a class which has an immutable partner class, the
 * object returned is an instance of that immutable class.<br />
 * The new object is <em>not</em> autoreleased, and is considered to be
 * 'owned' by the calling code ... which is therefore responsible for
 * releasing it.<br />
 * In the case where the receiver is an instance of a container class,
 * it is undefined whether contained objects are merely retained in the
 * new copy, or are themselves copied, or whether some other mechanism
 * entirely is used.
 */
- (id) copyWithZone: (NSZone*)zone;
@end

/**
 * This protocol must be adopted by any class wishing to support
 * mutable copying - ie where instances of the class should be able
 * to create mutable copies of themselves.
 */
@protocol NSMutableCopying
/**
 * Called by [NSObject-mutableCopy] passing NSDefaultMallocZone() as zone.<br />
 * This method returns a copy of the receiver and, where the receiver is an
 * immutable variant of a class which has a mutable partner class, the
 * object returned is an instance of that mutable class.
 * The new object is <em>not</em> autoreleased, and is considered to be
 * 'owned' by the calling code ... which is therefore responsible for
 * releasing it.<br />
 * In the case where the receiver is an instance of a container class,
 * it is undefined whether contained objects are merely retained in the
 * new copy, or are themselves copied, or whether some other mechanism
 * entirely is used.
 */
- (id) mutableCopyWithZone: (NSZone*)zone;
@end

/**
 * This protocol must be adopted by any class wishing to support
 * saving and restoring instances to an archive, or copying them
 * to remote processes via the Distributed Objects mechanism.
 */
@protocol NSCoding

/**
 * Called when it is time for receiver to be serialized for writing to an
 * archive or network connection.  Receiver should record all of its instance
 * variables using methods on aCoder.  See documentation for [NSCoder],
 * [NSArchiver], [NSKeyedArchiver], and/or [NSPortCoder] for more information.
 */
- (void) encodeWithCoder: (NSCoder*)aCoder;

/**
 * Called on a freshly allocated receiver when it is time to reconstitute from
 * serialized bytes in an archive or from a network connection.  Receiver
 * should load all of its instance variables using methods on aCoder.  See
 * documentation for [NSCoder], [NSUnarchiver], [NSKeyedUnarchiver], and/or
 * [NSPortCoder] for more information.
 */
- (id) initWithCoder: (NSCoder*)aDecoder;
@end

@protocol NSSecureCoding <NSCoding>
+ (BOOL)supportsSecureCoding;
@end


GS_ROOT_CLASS @interface NSObject <NSObject>
{
 /**
  * Points to instance's class.  Used by runtime to access method
  * implementations, etc..  Set in +alloc, Unlike other instance variables,
  * which are cleared there.
  */
  Class isa;
}

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
/** On a system which performs garbage collection, you should implement
 * this method to execute code when the receiver is collected.<br />
 * You must not call this method yourself (except when a subclass
 * calls the superclass method within its own implementation).
 */
- (void) finalize;
#endif

#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
- (NSString*) className;
#endif

+ (id) allocWithZone: (NSZone*)z;
+ (id) alloc;
+ (Class) class;

/**
 * This method is automatically invoked on any class which implements it
 * when the class is loaded into the runtime.<br />
 * It is also invoked on any category where the method is implemented
 * when that category is loaded into the runtime.<br />
 * The +load method is called directly by the runtime and you should never
 * send a +load message to a class yourself.<br />
 * This method is called <em>before</em> the +initialize message is sent
 * to the class, so you cannot depend on class initialisation having been
 * performed, or upon other classes existing (apart from superclasses of
 * the receiver, since +load is called on superclasses before it is called
 * on their subclasses).<br />
 * As a gross generalisation, it is safe to use C code, including
 * most ObjectiveC runtime functions within +load, but attempting to send
 * messages to ObjectiveC objects is likely to fail.<br />
 * In GNUstep, this method is implemented for NSObject to perform some
 * initialisation for the base library.<br />
 * If you implement +load for a class, don't call [super load] in your
 * implementation.
 */
+ (void) load;

/**
 * This message is automatically sent to a class by the runtime.  It is
 * sent once for each class, just before the class is used for the first
 * time (excluding any automatic call to +load by the runtime).<br />
 * The message is sent in a thread-safe manner ... other threads may not
 * call methods of the class until +initialize has finished executing.<br />
 * If the class has a superclass, its implementation of +initialize is
 * called first.<br />
 * If the class does not implement +initialize then the implementation
 * in the closest superclass may be called.  This means that +initialize may
 * be called more than once, and the recommended way to handle this by
 * using the
 * <code>
 * if (self == [classname class])
 * </code>
 * conditional to check whether the method is being called for a subclass.<br />
 * You should never call +initialize yourself ... let the runtime do it.<br />
 * You can implement +initialize in your own class if you need to.
 * NSObject's implementation handles essential root object and base
 * library initialization.<br />
 * It should be safe to call [super initialize] in your implementation
 * of +initialize.
 */
+ (void) initialize;
+ (IMP) instanceMethodForSelector: (SEL)aSelector;
+ (NSMethodSignature*) instanceMethodSignatureForSelector: (SEL)aSelector;
+ (BOOL) instancesRespondToSelector: (SEL)aSelector;
+ (BOOL) isSubclassOfClass: (Class)aClass;
+ (id) new;
+ (void) poseAsClass: (Class)aClassObject;
+ (id) setVersion: (NSInteger)aVersion;
+ (NSInteger) version;

- (id) awakeAfterUsingCoder: (NSCoder*)aDecoder;
- (Class) classForArchiver;
- (Class) classForCoder;
- (id) copy;
- (void) dealloc;
- (void) doesNotRecognizeSelector: (SEL)aSelector;
- (void) forwardInvocation: (NSInvocation*)anInvocation;
- (id) init;
- (IMP) methodForSelector: (SEL)aSelector;
- (NSMethodSignature*) methodSignatureForSelector: (SEL)aSelector;
- (id) mutableCopy;
- (id) replacementObjectForArchiver: (NSArchiver*)anArchiver;
- (id) replacementObjectForCoder: (NSCoder*)anEncoder;
- (Class) superclass;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
/**
 * This method will be called when attempting to send a message a class that
 * does not understand it.  The class may install a new method for the given
 * selector and return YES, otherwise it should return NO.
 *
 * Note: This method is only reliable when using the GNUstep runtime.  If you
 * require compatibility with the GCC runtime, you must also implement
 * -forwardInvocation: with equivalent semantics.  This will be considerably
 *  slower, but more portable.
 */
+ (BOOL) resolveClassMethod: (SEL)name;

/**
 * This method will be called when attempting to send a message an instance
 * that does not understand it.  The class may install a new method for the
 * given selector and return YES, otherwise it should return NO.
 *
 * Note: This method is only reliable when using the GNUstep runtime.  If you
 * require compatibility with the GCC runtime, you must also implement
 * -forwardInvocation: with equivalent semantics.  This will be considerably
 *  slower, but more portable.
 */
+ (BOOL) resolveInstanceMethod: (SEL)name;
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
/**
 * Returns an auto-accessing proxy for the given object.  This proxy sends a
 * -beginContentAccess message to the receiver when it is created and an
 * -endContentAccess message when it is destroyed.  This prevents an object
 * that implements the NSDiscardableContent protocol from having its contents
 * discarded for as long as the proxy exists.  
 *
 * On systems using the GNUstep runtime, messages send to the proxy will be
 * slightly slower than direct messages.  With the GCC runtime, they will be
 * approximately two orders of magnitude slower.  The GNUstep runtime,
 * therefore, is strongly recommended for code calling this method.
 */
- (id) autoContentAccessingProxy;

/**
 * If an object does not understand a message, it may delegate it to another
 * object.  Returning nil indicates that forwarding should not take place.  The
 * default implementation of this returns nil, but care should be taken when
 * subclassing NSObject subclasses and overriding this method that
 * the superclass implementation is called if returning nil.
 *
 * Note: This method is only reliable when using the GNUstep runtime and code
 * compiled with clang.  If you require compatibility with GCC and the GCC
 * runtime, you must also implement -forwardInvocation: with equivalent
 * semantics.  This will be considerably slower, but more portable.
 */
- (id) forwardingTargetForSelector: (SEL)aSelector;

#endif
@end

/**
 * Used to allocate memory to hold an object, and initialise the
 * class of the object to be aClass etc.  The allocated memory will
 * be extraBytes larger than the space actually needed to hold the
 * instance variables of the object.<br />
 * This function is used by the [NSObject+allocWithZone:] method.
 */
GS_EXPORT id
NSAllocateObject(Class aClass, NSUInteger extraBytes, NSZone *zone);

/**
 * Used to release the memory used by an object.<br />
 * This function is used by the [NSObject-dealloc] method.
 */
GS_EXPORT void
NSDeallocateObject(id anObject);

/**
 * Used to copy anObject.  This makes a bitwise copy of anObject to
 * memory allocated from zone.  The allocated memory will be extraBytes
 * longer than that necessary to actually store the instance variables
 * of the copied object.<br />
 */
GS_EXPORT NSObject *
NSCopyObject(NSObject *anObject, NSUInteger extraBytes, NSZone *zone);

/**
 * Returns a flag to indicate whether anObject should be retained or
 * copied in order to make a copy in the specified zone.<br />
 * Basically, this tests to see if anObject was allocated from
 * requestedZone and returns YES if it was.
 */
GS_EXPORT BOOL
NSShouldRetainWithZone(NSObject *anObject, NSZone *requestedZone);

GS_EXPORT BOOL
NSDecrementExtraRefCountWasZero(id anObject);

GS_EXPORT NSUInteger
NSExtraRefCount(id anObject);

GS_EXPORT void
NSIncrementExtraRefCount(id anObject);

#if OS_API_VERSION(GS_API_NONE, GS_API_NONE)

/** Global lock to be used by classes when operating on any global
    data that invoke other methods which also access global; thus,
    creating the potential for deadlock. */
GS_EXPORT NSRecursiveLock *gnustep_global_lock;

@interface NSObject (NEXTSTEP)
- (id) error:(const char *)aString, ...;
/* - (const char *) name;
   Removed because OpenStep has -(NSString*)name; */
@end

#if GS_API_VERSION(GS_API_NONE, 011700)
@interface NSObject (GNUstep)
+ (void) enableDoubleReleaseCheck: (BOOL)enable;
@end
#endif

#endif

#import	"NSDate.h"
/**
 *  Declares some methods for sending messages to self after a fixed delay.
 *  (These methods <em>are</em> in OpenStep and OS X.)
 */
@interface NSObject (TimedPerformers)

/**
 * Cancels any perform operations set up for the specified target
 * in the current run loop.
 */
+ (void) cancelPreviousPerformRequestsWithTarget: (id)obj;

/**
 * Cancels any perform operations set up for the specified target
 * in the current loop, but only if the value of aSelector and argument
 * with which the performs were set up match those supplied.<br />
 * Matching of the argument may be either by pointer equality or by
 * use of the [NSObject-isEqual:] method.
 */
+ (void) cancelPreviousPerformRequestsWithTarget: (id)obj
					selector: (SEL)s
					  object: (id)arg;
/**
 * Sets given message to be sent to this instance after given delay,
 * in any run loop mode.  See [NSRunLoop].
 */
- (void) performSelector: (SEL)s
	      withObject: (id)arg
	      afterDelay: (NSTimeInterval)seconds;

/**
 * Sets given message to be sent to this instance after given delay,
 * in given run loop modes.  See [NSRunLoop].
 */
- (void) performSelector: (SEL)s
	      withObject: (id)arg
	      afterDelay: (NSTimeInterval)seconds
		 inModes: (NSArray*)modes;
@end

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
/**
 * The NSDiscardableContent protocol is used by objects which encapsulate data
 * which may be discarded if resource constraints are exceeded.  These
 * constraints are typically, but not always, related memory.  
 */
@protocol NSDiscardableContent

/**
 * This method is called before any access to the object.  It returns YES if
 * the object's content is still valid.  The caller must call -endContentAccess
 * once for every call to -beginContentAccess;
 */
- (BOOL) beginContentAccess;

/**
 * Discards the contents of the object if it is not currently being edited.
 */
- (void) discardContentIfPossible;

/**
 * This method indicates that the caller has finished accessing the contents of
 * the object adopting this protocol.  Every call to -beginContentAccess must
 * be be paired with a call to this method after the caller has finished
 * accessing the contents.
 */
- (void) endContentAccess;

/**
 * Returns YES if the contents of the object have been discarded, either via a
 * call to -discardContentIfPossible while the object is not in use, or by some
 * implementation dependent mechanism.  
 */
- (BOOL) isContentDiscarded;
@end
#endif
#if	defined(__cplusplus)
}
#endif

#if     !NO_GNUSTEP && !defined(GNUSTEP_BASE_INTERNAL)
#import "../GNUstepBase/NSObject+GNUstepBase.h"
#endif

#endif /* __NSObject_h_GNUSTEP_BASE_INCLUDE */
