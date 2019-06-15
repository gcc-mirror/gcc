/* Interface for NSValue for GNUStep
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.

   Written by:  Adam Fedor <fedor@boulder.colorado.edu>
   Created: 1995
   
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

#ifndef __NSValue_h_GNUSTEP_BASE_INCLUDE
#define __NSValue_h_GNUSTEP_BASE_INCLUDE
#import	"../GNUstepBase/GSVersionMacros.h"

#import	"NSObject.h"
#import	"NSGeometry.h"
#import	"NSRange.h"

#if	defined(__cplusplus)
extern "C" {
#endif

@class NSString;

/**
 * The <code>NSValue</code> class can wrap a single primitive value as an
 * object so it can be used in the containers and other places where an object
 * reference is needed. Once initialized, an <code>NSValue</code> is
 * immutable, and there is no <code>NSMutableValue</code> class. You
 * initialize it by giving it a pointer to the primitive value, and you should
 * be careful this does not get freed until after the <code>NSValue</code> is
 * no longer used.
 */
@interface NSValue : NSObject <NSCopying, NSCoding>

// Allocating and Initializing 

/**
 * Create new instance with specified value (a pointer) of given type, which
 * is a string code obtainable through the compile-time operator
 * <code>@encode(...)</code>.  For example:
<example>
    NSValue *theValue = [NSValue value: &amp;n withObjCType: @encode(int)];
</example>
 */
+ (NSValue*) value: (const void*)value withObjCType: (const char*)type;

/**
 * Create new instance holding anObject.  This is useful if you want to add
 * anObject to a collection such as [NSArray] but don't want it to be retained
 * (a weak reference).
 */
+ (NSValue*) valueWithNonretainedObject: (id)anObject;

/**
 * Convenience method to create instance holding an <code>NSPoint</code>
 * structure.
 */
+ (NSValue*) valueWithPoint: (NSPoint)point;

/**
 * Convenience method to create instance holding a pointer.  Same as
 * using <code>@encode(void *)</code> in +value:withObjCType: .
 */
+ (NSValue*) valueWithPointer: (const void*)pointer;

/**
 * Convenience method to create instance holding an <code>NSRange</code>
 * structure.
 */
+ (NSValue*) valueWithRange: (NSRange)range;

/**
 * Convenience method to create instance holding an <code>NSRect</code>
 * structure.
 */
+ (NSValue*) valueWithRect: (NSRect)rect;

/**
 * Convenience method to create instance holding an <code>NSSize</code>
 * structure.
 */
+ (NSValue*) valueWithSize: (NSSize)size;

#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
/**
 * Synonym for value:withObjCType: .
 */
+ (NSValue*) valueWithBytes: (const void*)value objCType: (const char*)type;

/** <init/>
 * Initialize with value of type, parallel to value:withObjCType: .
 */
- (id) initWithBytes: (const void*)data objCType: (const char*)type;

/**
 * Compares this instance to another <code>NSValue</code>.  For equality,
 * both contents and declared type of the two values must match.
 */
- (BOOL) isEqualToValue: (NSValue*)other;
#endif	/* GS_API_MACOSX */

// Accessing Data 

/**
 * Copies bytes from the pointer receiver was initialized with into buffer
 * pointed to by value.  Number of bytes copied is determined by the type.  If
 * type was a void * pointer or object id, the memory address itself is
 * copied.
 */
- (void) getValue: (void*)value;

/**
 * Returns the string <code>@encode(...)</code> compatible type the receiver
 * was initialized with.
 */
- (const char*) objCType;

/**
 * If receiver was initialized with an object ID, return it, else raises
 * <code>NSInternalInconsistencyException</code>.
 */
- (id) nonretainedObjectValue;

/**
 * If receiver was initialized with a void * pointer, return it, else raises
 * <code>NSInternalInconsistencyException</code>.
 */
- (void*) pointerValue;

/**
 * If receiver was initialized with an <code>NSRange</code> value, return it,
 * else raises <code>NSInternalInconsistencyException</code>.
 */
- (NSRange) rangeValue;

/**
 * If receiver was initialized with an <code>NSRect</code> value, return it,
 * else raises <code>NSInternalInconsistencyException</code>.
 */
- (NSRect) rectValue;

/**
 * If receiver was initialized with an <code>NSSize</code> value, return it,
 * else raises <code>NSInternalInconsistencyException</code>.
 */
- (NSSize) sizeValue;

/**
 * If receiver was initialized with an <code>NSPoint</code> value, return it,
 * else raises <code>NSInternalInconsistencyException</code>.
 */
- (NSPoint) pointValue;

@end

/**
 * Subclass of [NSValue] offering convenience methods for initializing from
 * and accessing as any C primitive numeric type.  On access, the value will
 * be type-converted if necessary, using standard C conversion rules.
 */
@interface NSNumber : NSValue <NSCopying,NSCoding>

// Allocating and Initializing

/** New instance from boolean value. */
+ (NSNumber*) numberWithBool: (BOOL)value; 
/** New instance from signed char value. */
+ (NSNumber*) numberWithChar: (signed char)value;
/** New instance from double value. */
+ (NSNumber*) numberWithDouble: (double)value;
/** New instance from float value. */
+ (NSNumber*) numberWithFloat: (float)value;
/** New instance from (signed) int value. */
+ (NSNumber*) numberWithInt: (signed int)value;
/** New instance from (signed) long value. */
+ (NSNumber*) numberWithLong: (signed long)value;
/** New instance from (signed) long long value. */
+ (NSNumber*) numberWithLongLong: (signed long long)value;
/** New instance from (signed) short value. */
+ (NSNumber*) numberWithShort: (signed short)value;
/** New instance from unsigned char value. */
+ (NSNumber*) numberWithUnsignedChar: (unsigned char)value;
/** New instance from unsigned int value. */
+ (NSNumber*) numberWithUnsignedInt: (unsigned int)value;
/** New instance from unsigned long value. */
+ (NSNumber*) numberWithUnsignedLong: (unsigned long)value;
/** New instance from unsigned long long value. */
+ (NSNumber*) numberWithUnsignedLongLong: (unsigned long long)value;
/** New instance from unsigned short value. */
+ (NSNumber*) numberWithUnsignedShort: (unsigned short)value;

/** Initialize from boolean value. */
- (id) initWithBool: (BOOL)value;
/** Initialize from signed char value. */
- (id) initWithChar: (signed char)value;
/** Initialize from double value. */
- (id) initWithDouble: (double)value;
/** Initialize from float value. */
- (id) initWithFloat: (float)value;
/** Initialize from (signed) int value. */
- (id) initWithInt: (signed int)value;
/** Initialize from (signed) long value. */
- (id) initWithLong: (signed long)value;
/** Initialize from (signed) long long value. */
- (id) initWithLongLong: (signed long long)value;
/** Initialize from (signed) short value. */
- (id) initWithShort: (signed short)value;
/** Initialize from unsigned char value. */
- (id) initWithUnsignedChar: (unsigned char)value;
/** Initialize from unsigned int value. */
- (id) initWithUnsignedInt: (unsigned int)value;
/** Initialize from unsigned long value. */
- (id) initWithUnsignedLong: (unsigned long)value;
/** Initialize from unsigned long long value. */
- (id) initWithUnsignedLongLong: (unsigned long long)value;
/** Initialize from unsigned short value. */
- (id) initWithUnsignedShort: (unsigned short)value;

// Accessing Data 

/**
 * Return value as a BOOL; this will in fact be a char value converted
 * if necessary from type initialized with; if you wish to consider anything
 * nonzero TRUE do not compare directly to YES, but use <code>'!= NO'</code>.
 */
- (BOOL) boolValue;
/** Returns value as a signed char, converting if necessary. */
- (signed char) charValue;
/** Returns value as a double, converting if necessary. */
- (double) doubleValue;
/** Returns value as a float, converting if necessary. */
- (float) floatValue;
/** Returns value as a (signed) int, converting if necessary. */
- (signed int) intValue;
/** Returns value as a (signed) long, converting if necessary. */
- (signed long) longValue;
/** Returns value as a (signed) long long, converting if necessary. */
- (signed long long) longLongValue;
/** Returns value as a (signed) short, converting if necessary. */
- (signed short) shortValue;
/** Returns value as an unsigned char, converting if necessary. */
- (unsigned char) unsignedCharValue;
/** Returns value as an unsigned int, converting if necessary. */
- (unsigned int) unsignedIntValue;
/** Returns value as an unsigned long, converting if necessary. */
- (unsigned long) unsignedLongValue;
/** Returns value as an unsigned long long, converting if necessary. */
- (unsigned long long) unsignedLongLongValue;
/** Returns value as an unsigned short, converting if necessary. */
- (unsigned short) unsignedShortValue;

/** Returns -description . */
- (NSString*) stringValue;

/**
 * Returns the string representation of this number using a non-localised
 * conversion (decimal point is '.' irrespective of the locale).
 */
- (NSString*) description;

/**
 * <p>
 *   Produces a string representation of the number.  For a boolean
 *   this will be either 'true' or 'false'.  For other numbers the
 *   format is produced using the initWithFormat:locale:... method
 *   of NSString, and the format depends on the type of number as
 *   follows -
 * </p>
 * <deflist>
 *   <term>char</term>
 *   <desc>%i</desc>
 *   <term> short</term>
 *   <desc>%hi</desc>
 *   <term> int</term>
 *   <desc>%i</desc>
 *   <term> long</term>
 *   <desc>%li</desc>
 *   <term> long long</term>
 *   <desc>%lli</desc>
 *   <term> unsigned char</term>
 *   <desc>%u</desc>
 *   <term> unsigned short</term>
 *   <desc>%hu</desc>
 *   <term> unsigned int</term>
 *   <desc>%u</desc>
 *   <term> unsigned long</term>
 *   <desc>%lu</desc>
 *   <term> unsigned long long</term>
 *   <desc>%llu</desc>
 *   <term> float</term>
 *   <desc>%0.7g</desc>
 *   <term> double</term>
 *   <desc>%0.16g</desc>
 * </deflist>
 */
- (NSString*) descriptionWithLocale: (id)locale;

/**
 * Compares receiver with otherNumber, using C type conversion if necessary,
 * and returns <code>NSOrderedAscending</code>,
 * <code>NSOrderedDescending</code>, or <code>NSOrderedSame</code> depending
 * on whether it is less than, greater than, or equal to otherNumber.
 */
- (NSComparisonResult) compare: (NSNumber*)otherNumber;

/**
 * Returns whether receiver and otherNumber represent the same numerical value.
 */
- (BOOL) isEqualToNumber: (NSNumber*)otherNumber;


#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
/** Return a number intialised with NSInteger.
 */
+ (NSNumber*) numberWithInteger: (NSInteger)value;
/** Return a number intialised with NSUInteger.
 */
+ (NSNumber*) numberWithUnsignedInteger: (NSUInteger)value;
/** Initialise the receiver with NSInteger content.
 */
- (id) initWithInteger: (NSInteger)value;
/** Initialise the receiver with NSUInteger content.
 */
- (id) initWithUnsignedInteger: (NSUInteger)value;
/** Return the contents of the receiver as NSInteger.
 */
- (NSInteger) integerValue;
/** Return the contents of the receiver as NSUInteger.
 */
- (NSUInteger) unsignedIntegerValue;
#endif

@end

#if OS_API_VERSION(GS_API_NONE, GS_API_NONE)

/** Note: Defines a method that is not in the OpenStep spec, but makes
    subclassing easier. */
@interface NSValue (Subclassing)

/** Used by value: withObjCType: to determine the concrete subclass to alloc. */
+ (Class) valueClassWithObjCType: (const char*)type;

@end
#endif

#if	defined(__cplusplus)
}
#endif

#if     !NO_GNUSTEP && !defined(GNUSTEP_BASE_INTERNAL)
#import "../GNUstepBase/NSNumber+GNUstepBase.h"
#endif

#endif /* __NSValue_h_GNUSTEP_BASE_INCLUDE */
