/* CFBase.h
   
   Copyright (C) 2010 Free Software Foundation, Inc.
   
   Written by: Stefan Bidigaray
   Date: January, 2010
   
   This file is part of the GNUstep CoreBase Library.
   
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.         See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; see the file COPYING.LIB.
   If not, see <http://www.gnu.org/licenses/> or write to the 
   Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301, USA.
*/


#ifndef __COREFOUNDATION_CFBASE_H__
#define __COREFOUNDATION_CFBASE_H__

/* CoreFoundation defines __LITTLE_ENDIAN__ or __BIG_ENDIAN__ so we'll
 * do the same here for compatibility.
 */
#if !defined(__LITTLE_ENDIAN__) && !defined(__BIG_ENDIAN__)
#define __LITTLE_ENDIAN__ 1
#endif

#include "CFAvailability.h"
#include "../GNUstepBase/GSVersionMacros.h"

/*
 * CoreFoundation types
 */
typedef unsigned char Boolean;
typedef unsigned char UInt8;
typedef signed char SInt8;
typedef unsigned short UInt16;
typedef signed short SInt16;
typedef unsigned int UInt32;
typedef signed int SInt32;
typedef unsigned long long UInt64;
typedef signed long long SInt64;
typedef SInt32 OSStatus;

typedef float Float32;
typedef double Float64;
typedef UInt16 UniChar;
typedef UInt8 *StringPtr;
typedef const StringPtr *ConstStringPtr;
typedef UInt8 Str255[256];
typedef const Str255 *ConstStr255Param;
typedef SInt16 OSErr;
typedef SInt16 RegionCode;
typedef SInt16 LangCode;
typedef SInt16 ScriptCode;
typedef UInt32 FourCharCode;
#ifndef OSTYPE_DECLARED
typedef FourCharCode OSType;
#define OSTYPE_DECLARED
#endif
typedef UInt8 Byte;
typedef SInt8 SignedByte;

#ifndef UTF32Char               /* UTF32Char is also defined in GSConfig.h */
typedef UInt32 UTF32Char;
#endif
typedef UInt16 UTF16Char;
typedef UInt8 UTF8Char;

#if !defined(CF_EXTERN_C_BEGIN)
#if defined(__cplusplus)
#define CF_EXTERN_C_BEGIN extern "C" {
#define CF_EXTERN_C_END }
#else
#define CF_EXTERN_C_BEGIN
#define CF_EXTERN_C_END
#endif
#endif

#if defined(_WIN32)
#if defined(BUILDING_SELF)
#if defined(__cplusplus)
#define CF_EXPORT extern "C" __declspec(dllexport)
#else
#define CF_EXPORT extern __declspec(dllexport)
#endif
#else
#if defined(__cplusplus)
#define CF_EXPORT extern "C" __declspec(dllimport)
#else
#define CF_EXPORT extern __declspec(dllimport)
#endif
#endif
#else
#if defined(__cplusplus)
#define CF_EXPORT extern "C"
#else
#define CF_EXPORT extern
#endif
#endif

#if !defined(__bool_true_false_are_defined)
#define true 1
#define false 0
#endif

#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#if !defined(CF_INLINE)
#if defined(__GNUC__) && (__GNUC__ >= 4)
#define CF_INLINE static __inline__ __attribute__((always_inline))
#elif defined(__GNUC__)
#define CF_INLINE static __inline__
#elif defined(__MWERKS__) || defined(__cplusplus)
#define CF_INLINE static inline
#elif defined(_MSC_VER)
#define CF_INLINE static __inline
#elif _WIN32
#define CF_INLINE static __inline__
#else
#define CF_INLINE static inline
#endif
#endif

#if defined(__GNUC__) || defined(__llvm__)
#define GS_PURE_FUNCTION __attribute__((pure))
#else
#define GS_PURE_FUNCTION
#endif

CF_EXTERN_C_BEGIN
/** \defgroup CFTypeRef CFType Reference
    \{
 */
typedef unsigned long CFTypeID;
typedef const void *CFTypeRef;
/** @}
 */

/** \defgroup BaseUtils Base Utilities
    \{
 */
/** An integer value to store a hash code. */
typedef unsigned long CFHashCode;
/** A bitfield for passing information to functions.  Can hold as many bits
    as a word.
 */
typedef unsigned long CFOptionFlags;
/** A signed integer representing an index, size, length or count. */
typedef signed long CFIndex;

/** A structure that represents a range of items in a container, such as
    an array.
 */
typedef struct CFRange CFRange;
struct CFRange
{
  CFIndex location;
    /**< An integer representing the start location of the range, inclusive. */
  CFIndex length;
    /**< An integer representing the total number of items in the range */
};

/** Creates a CFRange structure.
    \param location The starting location.
    \param length The length.
    \return An initialized CFRange structure.
 */
CF_INLINE CFRange
CFRangeMake (CFIndex location, CFIndex length)
{
  CFRange range;

  range.location = location;
  range.length = length;
  return range;
}

/* Returned by comparison functions */
typedef enum
{
  kCFCompareLessThan = -1,
  kCFCompareEqualTo = 0,
  kCFCompareGreaterThan = 1
} CFComparisonResult;

/* Return when a value is not found */
enum
{
  kCFNotFound = -1
};

/* Definition for standard comparison function callback. */
typedef CFComparisonResult (*CFComparatorFunction) (const void *val1,
                                                    const void *val2,
                                                    void *context);

/* CoreFoundation version numbers */
/** \name Library Version Numbers
    \{
 */
CF_EXPORT const double kCFCoreFoundationVersionNumber;
#define kCFCoreFoundationVersionNumber10_0    196.40
#define kCFCoreFoundationVersionNumber10_0_3  196.50
#define kCFCoreFoundationVersionNumber10_1    226.00
#define kCFCoreFoundationVersionNumber10_1_1  226.00
#define kCFCoreFoundationVersionNumber10_1_2  227.20
#define kCFCoreFoundationVersionNumber10_1_3  227.20
#define kCFCoreFoundationVersionNumber10_1_4  227.30
#define kCFCoreFoundationVersionNumber10_2    263.00
#define kCFCoreFoundationVersionNumber10_2_1  263.10
#define kCFCoreFoundationVersionNumber10_2_2  263.10
#define kCFCoreFoundationVersionNumber10_2_3  263.30
#define kCFCoreFoundationVersionNumber10_2_4  263.30
#define kCFCoreFoundationVersionNumber10_2_5  263.50
#define kCFCoreFoundationVersionNumber10_2_6  263.50
#define kCFCoreFoundationVersionNumber10_2_7  263.50
#define kCFCoreFoundationVersionNumber10_2_8  263.50
#define kCFCoreFoundationVersionNumber10_3    299.00
#define kCFCoreFoundationVersionNumber10_3_1  299.00
#define kCFCoreFoundationVersionNumber10_3_2  299.00
#define kCFCoreFoundationVersionNumber10_3_3  299.30
#define kCFCoreFoundationVersionNumber10_3_4  299.31
#define kCFCoreFoundationVersionNumber10_3_5  299.31
#define kCFCoreFoundationVersionNumber10_3_6  299.32
#define kCFCoreFoundationVersionNumber10_3_7  299.33
#define kCFCoreFoundationVersionNumber10_3_8  299.33
#define kCFCoreFoundationVersionNumber10_3_9  299.35
#define kCFCoreFoundationVersionNumber10_4    368.00
#define kCFCoreFoundationVersionNumber10_4_1  368.10
#define kCFCoreFoundationVersionNumber10_4_2  368.11
#define kCFCoreFoundationVersionNumber10_4_3  368.18
#define kCFCoreFoundationVersionNumber10_4_4_Intel   368.26
#define kCFCoreFoundationVersionNumber10_4_4_PowerPC 368.25
#define kCFCoreFoundationVersionNumber10_4_5_Intel   368.26
#define kCFCoreFoundationVersionNumber10_4_5_PowerPC 368.25
#define kCFCoreFoundationVersionNumber10_4_6_Intel   368.26
#define kCFCoreFoundationVersionNumber10_4_6_PowerPC 368.25
#define kCFCoreFoundationVersionNumber10_4_7  368.27
#define kCFCoreFoundationVersionNumber10_4_8  368.27
#define kCFCoreFoundationVersionNumber10_4_9  368.28
#define kCFCoreFoundationVersionNumber10_4_10 368.28
#define kCFCoreFoundationVersionNumber10_4_11 368.31
#define kCFCoreFoundationVersionNumber10_5    476.00
#define kCFCoreFoundationVersionNumber10_5_1  476.00
#define kCFCoreFoundationVersionNumber10_5_2  476.10
#define kCFCoreFoundationVersionNumber10_5_3  476.13
#define kCFCoreFoundationVersionNumber10_5_4  476.14
#define kCFCoreFoundationVersionNumber10_5_5  476.15
#define kCFCoreFoundationVersionNumber10_5_6  476.17
/** \} */
/** \} */

#if __has_feature(attribute_cf_returns_retained)
#define CF_RETURNS_RETAINED __attribute__((cf_returns_retained))
#else
#define CF_RETURNS_RETAINED
#endif

#if __has_feature(attribute_cf_returns_not_retained)
#define CF_RETURNS_NOT_RETAINED __attribute__((cf_returns_not_retained))
#else
#define CF_RETURNS_NOT_RETAINED
#endif

/** \ingroup CFPropertyListRef
 */
typedef CFTypeRef CFPropertyListRef;

/** \ingroup CFStringRef
 */
typedef const struct __CFString *CFStringRef;
/** \ingroup CFMutableStringRef
 */
typedef struct __CFString *CFMutableStringRef;



/** \defgroup CFAllocatorRef CFAllocator Reference
    \brief CFAllocator is an opaque type used to allocate and deallocate
    memory.
    \{
 */
/** \brief A reference to a CFAllocator object.
 */
typedef const struct __CFAllocator *CFAllocatorRef;

typedef void *(*CFAllocatorAllocateCallBack) (CFIndex allocSize,
                                              CFOptionFlags hint, void *info);
typedef void (*CFAllocatorDeallocateCallBack) (void *ptr, void *info);
typedef void *(*CFAllocatorReallocateCallBack) (void *ptr,
                                                CFIndex newsize,
                                                CFOptionFlags hint, void *info);
typedef CFIndex (*CFAllocatorPreferredSizeCallBack) (CFIndex size,
                                                     CFOptionFlags hint,
                                                     void *info);
typedef const void *(*CFAllocatorRetainCallBack) (const void *info);
typedef void (*CFAllocatorReleaseCallBack) (const void *info);
typedef CFStringRef (*CFAllocatorCopyDescriptionCallBack) (const void *info);

struct _CFAllocatorContext
{
  CFIndex version;
  void *info;
  CFAllocatorRetainCallBack retain;
  CFAllocatorReleaseCallBack release;
  CFAllocatorCopyDescriptionCallBack copyDescription;
  CFAllocatorAllocateCallBack allocate;
  CFAllocatorReallocateCallBack reallocate;
  CFAllocatorDeallocateCallBack deallocate;
  CFAllocatorPreferredSizeCallBack preferredSize;
};
typedef struct _CFAllocatorContext CFAllocatorContext;

/** The default allocator and is equivalent to NULL.
    \see CFAllocatorGetDefault()
    \see CFAllocatorSetDefault()
 */
CF_EXPORT CFAllocatorRef kCFAllocatorDefault;
/** The default system allocator is used internally by GNUstep and is the
    default allocator if none is been defined.
    \see CFAllocatorSetDefault()
 */
CF_EXPORT CFAllocatorRef kCFAllocatorSystemDefault;
/** An allocator that uses the system's malloc, realloc and free functions.
 */
CF_EXPORT CFAllocatorRef kCFAllocatorMalloc;
#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
/** Equivalent to kCFAllocatorSystemDefault
 */
CF_EXPORT CFAllocatorRef kCFAllocatorMallocZone;
#endif
/** The NULL allocator does perform any operations.  Can be passed as
    a deallocator if you do not want GNUstep to deallocate the data.
 */
CF_EXPORT CFAllocatorRef kCFAllocatorNull;
/** This is a special case allocator directing CFAllocatorCreate() to use
    the given CFAllocatorContext structure to allocate the new allocator.
 */
CF_EXPORT CFAllocatorRef kCFAllocatorUseContext;

/** Create a new CFAllocator.
    \param allocator The allocator used to create this allocator or
      kCFAllocatorUseContext to use the functions in \b context.
    \param context The new allocator's context functions.
    \return A new CFAllocator or NULL in case of failure.
    \see CFAllocatorContext
 */
CF_EXPORT CFAllocatorRef
CFAllocatorCreate (CFAllocatorRef allocator, CFAllocatorContext * context);

/** Allocate new memory.
    \param allocator The CFAllocator to use.
    \param size The number of bytes to allocate.
    \param hint Option flags.  Currently unused and should be 0.
    \return Newly allocated memory of NULL in case of failure.
    \see CFAllocatorDeallocate()
 */
CF_EXPORT void *CFAllocatorAllocate (CFAllocatorRef allocator, CFIndex size,
                                     CFOptionFlags hint);

/** Deallocate the memory pointed to by \b ptr.
    \param allocator The CFAllocator to use.
    \param ptr A pointer previously allocated by CFAllocatorAllocate().
    \see CFAllocatorAllocate()
 */
CF_EXPORT void CFAllocatorDeallocate (CFAllocatorRef allocator, void *ptr);

CF_EXPORT CFIndex
CFAllocatorGetPreferredSizeForSize (CFAllocatorRef allocator, CFIndex size,
                                    CFOptionFlags hint);

CF_EXPORT void *CFAllocatorReallocate (CFAllocatorRef allocator, void *ptr,
                                       CFIndex newsize, CFOptionFlags hint);

CF_EXPORT CFAllocatorRef CFAllocatorGetDefault (void);

CF_EXPORT void CFAllocatorSetDefault (CFAllocatorRef allocator);

CF_EXPORT void
CFAllocatorGetContext (CFAllocatorRef allocator, CFAllocatorContext * context);

CF_EXPORT CFTypeID CFAllocatorGetTypeID (void);
/** \} */



/** \ingroup CFTypeRef
    \{
 */
/* These function will be implemented in CFRuntime.c since they 
   require runtime support. */
CF_EXPORT CFStringRef CFCopyDescription (CFTypeRef cf);

CF_EXPORT CFStringRef CFCopyTypeIDDescription (CFTypeID typeID);

CF_EXPORT Boolean CFEqual (CFTypeRef cf1, CFTypeRef cf2);

CF_EXPORT CFAllocatorRef CFGetAllocator (CFTypeRef cf);

CF_EXPORT CFIndex CFGetRetainCount (CFTypeRef cf);

CF_EXPORT CFTypeID CFGetTypeID (CFTypeRef cf);

CF_EXPORT CFHashCode CFHash (CFTypeRef cf);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_4, GS_API_LATEST)
CF_EXPORT CFTypeRef CFMakeCollectable (CFTypeRef cf);
#endif

CF_EXPORT void CFRelease (CFTypeRef cf);

CF_EXPORT CFTypeRef CFRetain (CFTypeRef cf);

CF_EXPORT CFTypeRef CFAutorelease(CFTypeRef arg);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_7, GS_API_LATEST)
CF_EXPORT void *_CFBridgingRelease (CFTypeRef cf);
CF_EXPORT CFTypeRef _CFBridgingRetain (void *obj);

#if __has_feature(objc_arc)
#define CFBridgingRetain(x) (__bridge_retained CFTypeRef)(x)
#define CFBridgingRelease(x) (__bridge_transfer id)(x)
#elif __OBJC__
#define CFBridgingRetain(x) _CFBridgingRetain((void *)(x))
#define CFBridgingRelease(x) (id)_CFBridgingRelease((x))
#else
#define CFBridgingRetain(x) _CFBridgingRetain((void *)(x))
#define CFBridgingRelease(x) _CFBridgingRelease((x))
#endif
#endif
/** \} */



/** \defgroup CFNullRef CFNull Reference
    \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
typedef struct __CFNull *CFNullRef;

CF_EXPORT CFNullRef kCFNull;

CFTypeID CFNullGetTypeID (void);
#endif
/** \} */

CF_EXTERN_C_END
#endif /* __COREFOUNDATION_CFBASE_H__ */
