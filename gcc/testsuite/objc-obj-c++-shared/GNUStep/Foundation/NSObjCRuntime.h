/** Interface to ObjC runtime for GNUStep
   Copyright (C) 1995, 1997, 2000 Free Software Foundation, Inc.

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

    AutogsdocSource: NSObjCRuntime.m
    AutogsdocSource: NSLog.m

   */

#ifndef __NSObjCRuntime_h_GNUSTEP_BASE_INCLUDE
#define __NSObjCRuntime_h_GNUSTEP_BASE_INCLUDE

/* Allow the elaborated enum use in _GS_NAMED_ENUM. */
#pragma GCC system_header

#ifdef __cplusplus
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif
#endif

#import	"../GNUstepBase/GSVersionMacros.h"
#import	"../GNUstepBase/GSConfig.h"
#import	"../GNUstepBase/GNUstep.h"
#if __BLOCKS__
#import	"../GNUstepBase/GSBlocks.h"
#endif

#include <stdarg.h>
#include <limits.h>
#include <float.h>

/* PA HP-UX kludge.  */
#if defined(__hppa__) && defined(__hpux__) && !defined(PRIuPTR)
#define PRIuPTR "lu"
#endif

/* IRIX kludge.  */
#if defined(__sgi)
/* IRIX 6.5 <inttypes.h> provides all definitions, but only for C99
   compilations.  */
#define PRId8 "hhd"
#define PRIu8 "hhu"
#if (_MIPS_SZLONG == 32)
#define PRId64 "lld"
#define PRIu64 "llu"
#endif
/* This doesn't match <inttypes.h>, which always has "lld" here, but the
   arguments are uint64_t, int64_t, which are unsigned long, long for
   64-bit in <sgidefs.h>.  */
#if (_MIPS_SZLONG == 64)
#define PRId64 "ld"
#define PRIu64 "lu"
#endif
/* This doesn't match <inttypes.h>, which has "u" here, but the arguments
   are uintptr_t, which is always unsigned long.  */
#define PRIuPTR "lu"
#endif

/* Solaris < 10 kludge.  */
#if defined(__sun__) && defined(__svr4__) && !defined(PRIuPTR)
#if defined(__arch64__) || defined (__x86_64__)
#define PRIuPTR "lu"
#define PRIdPTR "ld"
#define PRIxPTR "lx"
#else
#define PRIuPTR "u"
#define PRIdPTR "d"
#define PRIxPTR "x"
#endif
#endif


/* These typedefs must be in place before GSObjCRuntime.h is imported.
 */

#if     !defined(NSINTEGER_DEFINED)
typedef	intptr_t	NSInteger;
typedef	uintptr_t	NSUInteger;
#	define NSIntegerMax  INTPTR_MAX
#	define NSIntegerMin  INTPTR_MIN
#	define NSUIntegerMax UINTPTR_MAX
#endif /* !defined(NSINTEGER_DEFINED) */

#if     !defined(CGFLOAT_DEFINED)
#if     GS_SIZEOF_VOIDP == 8
#define CGFLOAT_IS_DBL  1
typedef double          CGFloat;
#define CGFLOAT_MIN	DBL_MIN
#define CGFLOAT_MAX	DBL_MAX
#else
typedef float           CGFloat;
#define CGFLOAT_MIN	FLT_MIN
#define CGFLOAT_MAX	FLT_MAX
#endif
#endif /* !defined(CGFLOAT_DEFINED) */

#define NSINTEGER_DEFINED 1
#define CGFLOAT_DEFINED 1
#ifndef NS_AUTOMATED_REFCOUNT_UNAVAILABLE
#  if __has_feature(objc_arc)
#    define NS_AUTOMATED_REFCOUNT_UNAVAILABLE \
      __attribute__((unavailable("Not available with automatic reference counting")))
#  else
#    define NS_AUTOMATED_REFCOUNT_UNAVAILABLE
#  endif
#endif


#if	defined(__cplusplus)
extern "C" {
#endif


/*
 * We can have strongly typed enums in C++11 mode or when the objc_fixed_enum
 * feature is availble.
 */
#if (__has_feature(objc_fixed_enum) || (__cplusplus && (__cplusplus > 199711L) && __has_extension(cxx_strong_enums)))
#  define _GS_NAMED_ENUM(ty, name) enum name : ty name; enum name : ty
#  define _GS_ANON_ENUM(ty) enum : ty
#  if __cplusplus
#    define NS_OPTIONS(ty,name) ty name; enum : ty
#  else
#    define NS_OPTIONS(ty,name) NS_ENUM(ty,name)
#  endif
#else // this provides less information, but works with older compilers
#  define _GS_NAMED_ENUM(ty, name) ty name; enum
#  define _GS_ANON_ENUM(ty) enum
#  define NS_OPTIONS(ty, name) NS_ENUM(ty, name)
#endif
// A bit of fairy dust to expand NS_ENUM to the correct variant
#define _GS_GET_ENUM_MACRO(_first,_second,NAME,...) NAME
/* The trick here is that placing the variadic args first will push the name
 * that the _GS_GET_ENUM_MACRO expands to into the correct position.
 */
#define NS_ENUM(...) _GS_GET_ENUM_MACRO(__VA_ARGS__,_GS_NAMED_ENUM,_GS_ANON_ENUM)(__VA_ARGS__)

/*
 * If the compiler supports nullability qualifiers, we define the macros for
 * non-null sections.
 */
#if __has_feature(nullability)
#  define NS_ASSUME_NONNULL_BEGIN _Pragma("clang assume_nonnull begin")
#  define NS_ASSUME_NONNULL_END   _Pragma("clang assume_nonnull end")
#else
#  define NS_ASSUME_NONNULL_BEGIN
#  define NS_ASSUME_NONNULL_END
#endif

/*
 * Backwards compatibility macro for instance type.
 */
#if !__has_feature(objc_instancetype)
# define instancetype id
#endif

/*
 * Backwards compatibility macros for Objective-C lightweight generics.
 */
#if __has_feature(objc_generics)
# define GS_GENERIC_CLASS(clz, ...) clz<__VA_ARGS__>
# define GS_GENERIC_TYPE_F(typeRef, fallback) typeRef
#else
# define GS_GENERIC_CLASS(clz, ...) clz
# define GS_GENERIC_TYPE_F(typeRef, fallback) fallback
#endif
#define GS_GENERIC_TYPE(typeRef) GS_GENERIC_TYPE_F(typeRef, id)

/**
 * Backwards compatibility macro for the objc_designated_initializer attribute
 */
#if __has_attribute(objc_designated_initializer)
#  define NS_DESIGNATED_INITIALIZER __attribute__((objc_designated_initializer))
#else
#  define NS_DESIGNATED_INITIALIZER
#endif

/** Bitfield used to specify options to control enumeration over collections.
 */
typedef NS_OPTIONS(NSUInteger, NSEnumerationOptions)
{
  NSEnumerationConcurrent = (1UL << 0), /** Specifies that the enumeration
   * is concurrency-safe.  Note that this does not mean that it will be
   * carried out in a concurrent manner, only that it can be.
   */

  NSEnumerationReverse = (1UL << 1) /** Specifies that the enumeration should
   * happen in the opposite of the natural order of the collection.
   */
};


/** Bitfield used to specify options to control the sorting of collections.
 */
typedef NS_OPTIONS(NSUInteger, NSSortOptions)
{
    NSSortConcurrent = (1UL << 0), /** Specifies that the sort
     * is concurrency-safe.  Note that this does not mean that it will be
     * carried out in a concurrent manner, only that it can be.
     */
    NSSortStable = (1UL << 4) /** Specifies that the sort should keep
     * equal objects in the same order in the collection.
     */
};


#import "../GNUstepBase/GSObjCRuntime.h"

#if OS_API_VERSION(MAC_OS_X_VERSION_10_5,GS_API_LATEST)
GS_EXPORT NSString	*NSStringFromProtocol(Protocol *aProtocol);
GS_EXPORT Protocol	*NSProtocolFromString(NSString *aProtocolName);
#endif
GS_EXPORT SEL		NSSelectorFromString(NSString *aSelectorName);
GS_EXPORT NSString	*NSStringFromSelector(SEL aSelector);
GS_EXPORT SEL		NSSelectorFromString(NSString *aSelectorName);
GS_EXPORT Class		NSClassFromString(NSString *aClassName);
GS_EXPORT NSString	*NSStringFromClass(Class aClass);
GS_EXPORT const char	*NSGetSizeAndAlignment(const char *typePtr,
  NSUInteger *sizep, NSUInteger *alignp);

#if OS_API_VERSION(GS_API_NONE, GS_API_NONE)
/* Logging */
/**
 *  OpenStep spec states that log messages go to stderr, but just in case
 *  someone wants them to go somewhere else, they can implement a function
 *  like this and assign a pointer to it to _NSLog_printf_handler.
 */
typedef void NSLog_printf_handler (NSString* message);
GS_EXPORT NSLog_printf_handler	*_NSLog_printf_handler;
GS_EXPORT int	_NSLogDescriptor;
@class NSRecursiveLock;
GS_EXPORT NSRecursiveLock	*GSLogLock(void);
#endif

GS_EXPORT void	NSLog(NSString *format, ...) NS_FORMAT_FUNCTION(1,2);
GS_EXPORT void	NSLogv(NSString *format, va_list args) NS_FORMAT_FUNCTION(1,0);

#ifndef YES
#define YES		1
#endif
#ifndef NO
#define NO		0
#endif
#ifndef nil
#define nil		0
#endif

/**
 * Contains values <code>NSOrderedSame</code>, <code>NSOrderedAscending</code>
 * <code>NSOrderedDescending</code>, for left hand side equals, less than, or
 * greater than right hand side.
 */
typedef NS_ENUM(NSInteger, NSComparisonResult)
{
  NSOrderedAscending = (NSInteger)-1, NSOrderedSame, NSOrderedDescending
};

enum {NSNotFound = NSIntegerMax};

#if __BLOCKS__
DEFINE_BLOCK_TYPE(NSComparator, NSComparisonResult, id, id);
#endif

/**
 * Declare the foundation export macro as an alias to GS_EXPORT 
 */
#define FOUNDATION_EXPORT GS_EXPORT

#if	defined(__cplusplus)
}
#endif

/**
 * Declare Apple availability macros for compatibility purposes as no-ops.
 */
#define NS_CLASS_AVAILABLE(...)
#define NS_AVAILABLE(...)
#define NS_AVAILABLE_MAC(...)
#define NS_DEPRECATED(...)
#define NS_DEPRECATED_MAC(...)
#define NS_ENUM_AVAILABLE(...)
#define NS_ENUM_AVAILABLE_MAC(...)
#define NS_ENUM_DEPRECATED(...)
#define NS_ENUM_DEPRECATED_MAC(...)
#define NS_CLASS_AVAILABLE(...)
#define NS_CLASS_DEPRECATED(...)
#define NS_CLASS_AVAILABLE_MAC(...)
#define NS_CLASS_DEPRECATED_MAC(...)
#define NS_UNAVAILABLE

/* Define root class NS macro */
#ifndef NS_ROOT_CLASS
#if __has_attribute(objc_root_class)
#define NS_ROOT_CLASS __attribute__((objc_root_class))
#else
#define NS_ROOT_CLASS
#endif
#endif

#endif /* __NSObjCRuntime_h_GNUSTEP_BASE_INCLUDE */
