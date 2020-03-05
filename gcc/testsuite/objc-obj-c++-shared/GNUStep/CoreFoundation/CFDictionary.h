/* CFDictionary.h

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
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; see the file COPYING.LIB.
   If not, see <http://www.gnu.org/licenses/> or write to the 
   Free Software Foundation, 51 Franklin Street, Fifth Floor, 
   Boston, MA 02110-1301, USA.
*/

#ifndef __COREFOUNDATION_CFDICTIONARY_H__
#define __COREFOUNDATION_CFDICTIONARY_H__ 1

#include "CFBase.h"

CF_EXTERN_C_BEGIN
/** \ingroup CFDictionaryRef */
typedef const struct __CFDictionary *CFDictionaryRef;
/** \ingroup CFMutableDictionaryRef */
typedef struct __CFDictionary *CFMutableDictionaryRef;

/** \defgroup CFDictionaryRef CFDictionary Reference
    \{
 */
typedef void (*CFDictionaryApplierFunction) (const void *key,
                                             const void *value, void *context);

typedef CFStringRef (*CFDictionaryCopyDescriptionCallBack) (const void *value);
typedef Boolean (*CFDictionaryEqualCallBack) (const void *value1,
                                              const void *value2);
typedef CFHashCode (*CFDictionaryHashCallBack) (const void *value);
typedef void (*CFDictionaryReleaseCallBack) (CFAllocatorRef allocator,
                                             const void *value);
typedef const void *(*CFDictionaryRetainCallBack) (CFAllocatorRef allocator,
                                                   const void *value);

typedef struct _CFDictionaryKeyCallBacks CFDictionaryKeyCallBacks;
struct _CFDictionaryKeyCallBacks
{
  CFIndex version;
  CFDictionaryRetainCallBack retain;
  CFDictionaryReleaseCallBack release;
  CFDictionaryCopyDescriptionCallBack copyDescription;
  CFDictionaryEqualCallBack equal;
  CFDictionaryHashCallBack hash;
};

typedef struct _CFDictionaryValueCallBacks CFDictionaryValueCallBacks;
struct _CFDictionaryValueCallBacks
{
  CFIndex version;
  CFDictionaryRetainCallBack retain;
  CFDictionaryReleaseCallBack release;
  CFDictionaryCopyDescriptionCallBack copyDescription;
  CFDictionaryEqualCallBack equal;
};

CF_EXPORT const CFDictionaryKeyCallBacks kCFCopyStringDictionaryKeyCallBacks;
CF_EXPORT const CFDictionaryKeyCallBacks kCFTypeDictionaryKeyCallBacks;
CF_EXPORT const CFDictionaryValueCallBacks kCFTypeDictionaryValueCallBacks;

/** \name Creating a dictionary
    \{
 */
CF_EXPORT CFDictionaryRef
CFDictionaryCreate (CFAllocatorRef allocator, const void **keys,
                    const void **values, CFIndex numValues,
                    const CFDictionaryKeyCallBacks * keyCallBacks,
                    const CFDictionaryValueCallBacks * valueCallBacks);

CF_EXPORT CFDictionaryRef
CFDictionaryCreateCopy (CFAllocatorRef allocator, CFDictionaryRef theDict);
/** \} */

/** \name Examining a dictionary
    \{
 */
CF_EXPORT Boolean
CFDictionaryContainsKey (CFDictionaryRef theDict, const void *key);

CF_EXPORT Boolean
CFDictionaryContainsValue (CFDictionaryRef theDict, const void *value);

CF_EXPORT CFIndex CFDictionaryGetCount (CFDictionaryRef theDict);

CF_EXPORT CFIndex
CFDictionaryGetCountOfKey (CFDictionaryRef theDict, const void *key);

CF_EXPORT CFIndex
CFDictionaryGetCountOfValue (CFDictionaryRef theDict, const void *value);

CF_EXPORT void
CFDictionaryGetKeysAndValues (CFDictionaryRef theDict, const void **keys,
                              const void **values);

CF_EXPORT const void *CFDictionaryGetValue (CFDictionaryRef theDict,
                                            const void *key);

CF_EXPORT Boolean
CFDictionaryGetValueIfPresent (CFDictionaryRef theDict, const void *key,
                               const void **value);
/** \} */

/** \name Applying a funcation to a dictionary
    \{
 */
CF_EXPORT void
CFDictionaryApplyFunction (CFDictionaryRef theDict,
                           CFDictionaryApplierFunction applier, void *context);
/** \} */

/** \name Getting the CFDictionary type ID
    \{
 */
CF_EXPORT CFTypeID CFDictionaryGetTypeID (void);
/** \} */
/** \} */

/** \defgroup CFMutableDictionaryRef CFMutableDictionary Reference
    \{
 */
/** \name Creating a Mutable Dictionary
    \{
 */
CF_EXPORT CFMutableDictionaryRef
CFDictionaryCreateMutable (CFAllocatorRef allocator, CFIndex capacity,
                           const CFDictionaryKeyCallBacks * keyCallBacks,
                           const CFDictionaryValueCallBacks * valueCallBacks);

CF_EXPORT CFMutableDictionaryRef
CFDictionaryCreateMutableCopy (CFAllocatorRef allocator, CFIndex capacity,
                               CFDictionaryRef theDict);
/** \} */

/** \name Modifying a Dictionary
    \{
 */
CF_EXPORT void
CFDictionaryAddValue (CFMutableDictionaryRef theDict, const void *key,
                      const void *value);

CF_EXPORT void CFDictionaryRemoveAllValues (CFMutableDictionaryRef theDict);

CF_EXPORT void
CFDictionaryRemoveValue (CFMutableDictionaryRef theDict, const void *key);

CF_EXPORT void
CFDictionaryReplaceValue (CFMutableDictionaryRef theDict, const void *key,
                          const void *value);

CF_EXPORT void
CFDictionarySetValue (CFMutableDictionaryRef theDict, const void *key,
                      const void *value);
/** \} */
/** \} */

CF_EXTERN_C_END
#endif /* __COREFOUNDATION_CFDICTIONARY_H__ */
