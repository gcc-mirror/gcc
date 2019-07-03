/* Interface for NSArray for GNUStep
   Copyright (C) 1995-2015 Free Software Foundation, Inc.

   Written by:  Andrew Kachites McCallum <mccallum@gnu.ai.mit.edu>
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

#ifndef __NSArray_h_GNUSTEP_BASE_INCLUDE
#define __NSArray_h_GNUSTEP_BASE_INCLUDE
#import	"../GNUstepBase/GSVersionMacros.h"

#import	"NSObject.h"
#import	"NSRange.h"
#import "NSEnumerator.h"
#if __BLOCKS__
#import "../GNUstepBase/GSBlocks.h"
#endif

#if	defined(__cplusplus)
extern "C" {
#endif

@class NSString;
@class NSURL;
@class NSIndexSet;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
enum
{
  NSBinarySearchingFirstEqual = (1UL << 8), /** Specifies that the binary
   * search should find the first object equal in the array.
   */
  NSBinarySearchingLastEqual = (1UL << 9), /** Specifies that the binary
   * search should find the last object equal in the array.
   */
  NSBinarySearchingInsertionIndex = (1UL << 10), /** Specifies that the binary
   * search should find the index at which an equal object should be inserted
   * in order to keep the array sorted
   */
};

typedef NSUInteger NSBinarySearchingOptions;
#endif

@interface GS_GENERIC_CLASS(NSArray, __covariant ElementT) : NSObject
  <NSCoding, NSCopying, NSMutableCopying, NSFastEnumeration>

+ (instancetype) array;
+ (instancetype) arrayWithArray: (GS_GENERIC_CLASS(NSArray, ElementT) *)array;
+ (instancetype) arrayWithContentsOfFile: (NSString*)file;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
+ (instancetype) arrayWithContentsOfURL: (NSURL*)aURL;
#endif
+ (instancetype) arrayWithObject: (id)anObject;
+ (instancetype) arrayWithObjects: (id)firstObject, ...;
+ (instancetype) arrayWithObjects: (const id[])objects count: (NSUInteger)count;

- (GS_GENERIC_CLASS(NSArray, ElementT) *) arrayByAddingObject:
  (GS_GENERIC_TYPE(ElementT))anObject;
- (GS_GENERIC_CLASS(NSArray, ElementT) *) arrayByAddingObjectsFromArray:
  (GS_GENERIC_CLASS(NSArray, ElementT)*)anotherArray;
- (BOOL) containsObject: (GS_GENERIC_TYPE(ElementT))anObject;

/** <override-subclass />
 * Returns the number of elements contained in the receiver.
 */
- (NSUInteger) count;
- (void) getObjects: (__unsafe_unretained GS_GENERIC_TYPE(ElementT)[])aBuffer;
- (void) getObjects: (__unsafe_unretained GS_GENERIC_TYPE(ElementT)[])aBuffer
              range: (NSRange)aRange;
- (NSUInteger) indexOfObject: (GS_GENERIC_TYPE(ElementT))anObject;
- (NSUInteger) indexOfObject: (GS_GENERIC_TYPE(ElementT))anObject
                     inRange: (NSRange)aRange;
- (NSUInteger) indexOfObjectIdenticalTo: (GS_GENERIC_TYPE(ElementT))anObject;
- (NSUInteger) indexOfObjectIdenticalTo: (GS_GENERIC_TYPE(ElementT))anObject
                                inRange: (NSRange)aRange;
- (instancetype) init;
- (instancetype) initWithArray: (GS_GENERIC_CLASS(NSArray, ElementT)*)array;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
- (instancetype) initWithArray: (GS_GENERIC_CLASS(NSArray, ElementT)*)array
                     copyItems: (BOOL)shouldCopy;
#endif
- (instancetype) initWithContentsOfFile: (NSString*)file;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
- (instancetype) initWithContentsOfURL: (NSURL*)aURL;
#endif
- (instancetype) initWithObjects: (GS_GENERIC_TYPE(ElementT)) firstObject, ...;

/** <init /> <override-subclass />
 * This should initialize the array with count (may be zero) objects.<br />
 * Retains each object placed in the array.<br />
 * Calls -init (which does nothing but maintain MacOS-X compatibility),
 * and needs to be re-implemented in subclasses in order to have all
 * other initialisers work.
 */
- (instancetype) initWithObjects: (const GS_GENERIC_TYPE(ElementT)[])objects
                           count: (NSUInteger)count;
- (GS_GENERIC_TYPE(ElementT)) lastObject;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
- (GS_GENERIC_TYPE(ElementT)) firstObject;
#endif

/** <override-subclass />
 * Returns the object at the specified index.
 * Raises an exception of the index is beyond the array.
 */
- (GS_GENERIC_TYPE(ElementT)) objectAtIndex: (NSUInteger)index;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
- (GS_GENERIC_CLASS(NSArray, ElementT) *) objectsAtIndexes:
  (NSIndexSet *)indexes;
#endif

- (GS_GENERIC_TYPE(ElementT)) firstObjectCommonWithArray:
    (GS_GENERIC_CLASS(NSArray, ElementT) *)otherArray;
- (BOOL) isEqualToArray: (NSArray*)otherArray;

#if OS_API_VERSION(GS_API_OPENSTEP, GS_API_MACOSX)
- (void) makeObjectsPerform: (SEL)aSelector;
- (void) makeObjectsPerform: (SEL)aSelector withObject: (id)argument;
#endif
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
- (void) makeObjectsPerformSelector: (SEL)aSelector;
- (void) makeObjectsPerformSelector: (SEL)aSelector withObject: (id)arg;
#endif

- (NSData*) sortedArrayHint;
- (GS_GENERIC_CLASS(NSArray, ElementT)*) sortedArrayUsingFunction:
    (NSComparisonResult (*)(id, id, void*))comparator
			        context: (void*)context;
- (GS_GENERIC_CLASS(NSArray, ElementT)*) sortedArrayUsingFunction:
    (NSComparisonResult (*)(id, id, void*))comparator
			      context: (void*)context
				     hint: (NSData*)hint;
- (GS_GENERIC_CLASS(NSArray, ElementT)*) sortedArrayUsingSelector:
  (SEL)comparator;
- (GS_GENERIC_CLASS(NSArray, ElementT)*) subarrayWithRange: (NSRange)aRange;

- (NSString*) componentsJoinedByString: (NSString*)separator;
- (GS_GENERIC_CLASS(NSArray, NSString*)*) pathsMatchingExtensions:
    (GS_GENERIC_CLASS(NSArray, NSString*)*)extensions;

- (GS_GENERIC_CLASS(NSEnumerator, ElementT)*) objectEnumerator;
- (GS_GENERIC_CLASS(NSEnumerator, ElementT)*) reverseObjectEnumerator;

- (NSString*) description;
- (NSString*) descriptionWithLocale: (id)locale;
- (NSString*) descriptionWithLocale: (id)locale
			     indent: (NSUInteger)level;

- (BOOL) writeToFile: (NSString*)path atomically: (BOOL)useAuxiliaryFile;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
- (BOOL) writeToURL: (NSURL*)url atomically: (BOOL)useAuxiliaryFile;
- (GS_GENERIC_TYPE(ElementT)) valueForKey: (NSString*)key;
- (void) setValue: (GS_GENERIC_TYPE(ElementT))value forKey: (NSString*)key;
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)

#if __BLOCKS__
DEFINE_BLOCK_TYPE(GSEnumeratorBlock, void, GS_GENERIC_TYPE(ElementT),
  NSUInteger, BOOL*);
DEFINE_BLOCK_TYPE(GSPredicateBlock, BOOL, GS_GENERIC_TYPE(ElementT),
  NSUInteger, BOOL*);
/**
 * Enumerate over the collection using the given block.  The first argument is
 * the object and the second is the index in the array.  The final argument is
 * a pointer to a BOOL indicating whether the enumeration should stop.  Setting
 * this to YES will interrupt the enumeration.
 */
- (void) enumerateObjectsUsingBlock: (GSEnumeratorBlock)aBlock;

/**
 * Enumerate over the collection using the given block.  The first argument is
 * the object and the second is the index in the array.  The final argument is
 * a pointer to a BOOL indicating whether the enumeration should stop.  Setting
 * this to YES will interrupt the enumeration.
 *
 * The opts argument is a bitfield.  Setting the NSNSEnumerationConcurrent flag
 * specifies that it is thread-safe.  The NSEnumerationReverse bit specifies
 * that it should be enumerated in reverse order.
 */
- (void) enumerateObjectsWithOptions: (NSEnumerationOptions)opts
			  usingBlock: (GSEnumeratorBlock)aBlock;
/**
 * Enumerate over the specified indexes in the collection using the given
 * block.  The first argument is the object and the second is the index in the
 * array.  The final argument is a pointer to a BOOL indicating whether the
 * enumeration should stop.  Setting this to YES will interrupt the
 * enumeration.
 *
 * The opts argument is a bitfield.  Setting the NSNSEnumerationConcurrent flag
 * specifies that it is thread-safe.  The NSEnumerationReverse bit specifies
 * that it should be enumerated in reverse order.
 */
- (void) enumerateObjectsAtIndexes: (NSIndexSet*)indexSet
			   options: (NSEnumerationOptions)opts
			usingBlock: (GSEnumeratorBlock)block;
/**
 * Returns the indexes of the objects in a collection that match the condition
 * specified by the block.
 *
 * The opts argument is a bitfield.  Setting the NSNSEnumerationConcurrent flag
 * specifies that it is thread-safe.  The NSEnumerationReverse bit specifies
 * that it should be enumerated in reverse order.
 */
- (NSIndexSet *) indexesOfObjectsWithOptions: (NSEnumerationOptions)opts
				 passingTest: (GSPredicateBlock)predicate;

/**
 * Returns the indexes of the objects in a collection that match the condition
 * specified by the block.
 */
- (NSIndexSet*) indexesOfObjectsPassingTest: (GSPredicateBlock)predicate;

/**
 * Returns the indexes of the objects in a collection that match the condition
 * specified by the block and are in the range specified by the index set.
 *
 * The opts argument is a bitfield.  Setting the NSNSEnumerationConcurrent flag
 * specifies that it is thread-safe.  The NSEnumerationReverse bit specifies
 * that it should be enumerated in reverse order.
 */
- (NSIndexSet*) indexesOfObjectsAtIndexes: (NSIndexSet*)indexSet
				  options: (NSEnumerationOptions)opts
			      passingTest: (GSPredicateBlock)predicate;

/**
 * Returns the index of the first object in the array that matches the
 * condition specified by the block.
 *
 * The opts argument is a bitfield.  Setting the NSNSEnumerationConcurrent flag
 * specifies that it is thread-safe.  The NSEnumerationReverse bit specifies
 * that it should be enumerated in reverse order.
 */
- (NSUInteger) indexOfObjectWithOptions: (NSEnumerationOptions)opts
			    passingTest: (GSPredicateBlock)predicate;

/**
 * Returns the index of the first object in the array that matches the
 * condition specified by the block.
 */
- (NSUInteger) indexOfObjectPassingTest: (GSPredicateBlock)predicate;

/**
 * Returns the index of the first object in the specified range in a collection
 * that matches the condition specified by the block.
 *
 * The opts argument is a bitfield.  Setting the NSNSEnumerationConcurrent flag
 * specifies that it is thread-safe.  The NSEnumerationReverse bit specifies
 * that it should be enumerated in reverse order.
 */
- (NSUInteger) indexOfObjectAtIndexes: (NSIndexSet*)indexSet
			      options: (NSEnumerationOptions)opts
			  passingTest: (GSPredicateBlock)predicate;

/** Returns a sorted array using the comparator to determine the
 * order of objects.
 */
- (GS_GENERIC_CLASS(NSArray, ElementT) *) sortedArrayUsingComparator:
    (NSComparator)comparator;

/** Returns a sorted array using the block to determine the order of objects.
 *
 * The opts argument is a bitfield.  Setting the NSSortConcurrent flag
 * specifies that it is thread-safe.  The NSSortStable bit specifies that
 * it should keep equal objects in the same order.
 */
- (GS_GENERIC_CLASS(NSArray, ElementT) *)
    sortedArrayWithOptions: (NSSortOptions)options
           usingComparator: (NSComparator)comparator;

/**
 * Performs a binary search of the array within the specified range for the
 * index of an object equal to obj according to cmp.
 * If NSBinarySearchingInsertionIndex is specified, searches for the index
 * at which such an object should be inserted.
 */
- (NSUInteger) indexOfObject: (id)key
               inSortedRange: (NSRange)range
                     options: (NSBinarySearchingOptions)options
             usingComparator: (NSComparator)comparator;
#endif
#endif
/**
 * Accessor for subscripting.  This is called by the compiler when you write
 * code like anArray[12].  It should not be called directly.
 */
- (GS_GENERIC_TYPE(ElementT)) objectAtIndexedSubscript: (NSUInteger)anIndex;
@end


@interface GS_GENERIC_CLASS(NSMutableArray, ElementT) : NSArray

+ (instancetype) arrayWithCapacity: (NSUInteger)numItems;

/** <override-subclass />
 * Adds anObject at the end of the array, thus increasing the size of
 * the array.  The object is retained upon addition.
 */
- (void) addObject: (GS_GENERIC_TYPE(ElementT))anObject;
- (void) addObjectsFromArray: (GS_GENERIC_CLASS(NSArray, ElementT)*)otherArray;
#if OS_API_VERSION(GS_API_MACOSX, GS_API_LATEST)
- (void) exchangeObjectAtIndex: (NSUInteger)i1
	     withObjectAtIndex: (NSUInteger)i2;
#endif

/** <init /> <override-subclass />
 * Initialise the array with the specified capacity ... this
 * should ensure that the array can have numItems added efficiently.<br />
 * Calls -init (which does nothing but maintain MacOS-X compatibility),
 * and needs to be re-implemented in subclasses in order to have all
 * other initialisers work.
 */
- (instancetype) initWithCapacity: (NSUInteger)numItems;

/** <override-subclass />
 * Inserts an object into the receiver at the specified location.<br />
 * Raises an exception if given an array index which is too large.<br />
 * The size of the array increases by one.<br />
 * The object is retained by the array.
 */
- (void) insertObject: (GS_GENERIC_TYPE(ElementT))anObject
              atIndex: (NSUInteger)index;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
- (void) insertObjects: (GS_GENERIC_CLASS(NSArray, ElementT) *)objects
             atIndexes: (NSIndexSet *)indexes;
#endif

/** <override-subclass />
 * Removes an object from the receiver at the specified location.<br />
 * The size of the array decreases by one.<br />
 * Raises an exception if given an array index which is too large.<br />
 */
- (void) removeObjectAtIndex: (NSUInteger)index;

- (void) removeObjectsAtIndexes: (NSIndexSet *)indexes;

/** <override-subclass />
 * Places an object into the receiver at the specified location.<br />
 * Raises an exception if given an array index which is too large.<br />
 * The object is retained by the array.
 */
- (void) replaceObjectAtIndex: (NSUInteger)index
		   withObject: (GS_GENERIC_TYPE(ElementT))anObject;

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
- (void) replaceObjectsAtIndexes: (NSIndexSet *)indexes
                     withObjects: (GS_GENERIC_CLASS(NSArray, ElementT)*)objects;
#endif

- (void) replaceObjectsInRange: (NSRange)aRange
          withObjectsFromArray: (GS_GENERIC_CLASS(NSArray, ElementT)*)anArray;

- (void) replaceObjectsInRange: (NSRange)aRange
          withObjectsFromArray: (GS_GENERIC_CLASS(NSArray, ElementT)*)anArray
                         range: (NSRange)anotherRange;

- (void) setArray: (GS_GENERIC_CLASS(NSArray, ElementT) *)otherArray;

- (void) removeAllObjects;
- (void) removeLastObject;
- (void) removeObject: (GS_GENERIC_TYPE(ElementT))anObject;
- (void) removeObject: (GS_GENERIC_TYPE(ElementT))anObject
              inRange: (NSRange)aRange;
- (void) removeObjectIdenticalTo: (GS_GENERIC_TYPE(ElementT))anObject;
- (void) removeObjectIdenticalTo: (GS_GENERIC_TYPE(ElementT))anObject
                         inRange: (NSRange)aRange;
- (void) removeObjectsInArray: (GS_GENERIC_CLASS(NSArray, ElementT)*)otherArray;
- (void) removeObjectsInRange: (NSRange)aRange;
- (void) removeObjectsFromIndices: (NSUInteger*)indices
		       numIndices: (NSUInteger)count;

- (void) sortUsingFunction:
    (NSComparisonResult (*)(GS_GENERIC_TYPE(ElementT),
       GS_GENERIC_TYPE(ElementT),void*))compare
		           context: (void*)context;
- (void) sortUsingSelector: (SEL)comparator;


#if OS_API_VERSION(MAC_OS_X_VERSION_10_6, GS_API_LATEST)
#if __BLOCKS__
/**
 * Sorts the array using the specified comparator block.
 */
- (void) sortUsingComparator: (NSComparator)comparator;

/**
 * Sorts the array using the specified comparator block and options.
 */
- (void) sortWithOptions: (NSSortOptions)options
         usingComparator: (NSComparator)comparator;
#endif
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_8, GS_API_LATEST)
/** Set method called by the compiler with array subscripting.<br />
 * Replaces the object at anIndex or, if anIndex is the length of the array,
 * this method appends abObject to the array.
 */
- (void) setObject: (GS_GENERIC_TYPE(ElementT))anObject
atIndexedSubscript: (NSUInteger)anIndex;
#endif
@end

#if	defined(__cplusplus)
}
#endif

#if	!NO_GNUSTEP && !defined(GNUSTEP_BASE_INTERNAL)
#import	"../GNUstepBase/NSArray+GNUstepBase.h"
#endif

#endif /* __NSArray_h_GNUSTEP_BASE_INCLUDE */
