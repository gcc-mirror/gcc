/* CFArray.h
   
   Copyright (C) 2010 Free Software Foundation, Inc.
   
   Written by: Stefan Bidigaray
   Date: January, 2010
   
   This file is part of CoreBase.
   
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


#ifndef __COREFOUNDATION_CFARRAY_H__
#define __COREFOUNDATION_CFARRAY_H__

#include "CFBase.h"

CF_EXTERN_C_BEGIN
/** \ingroup CFArrayRef
    \brief Reference to an immutable array object.
 */
typedef const struct __CFArray *CFArrayRef;
/**
    \ingroup CFMutableArrayRef
    \brief Reference to a mutable array object.
 */
typedef struct __CFArray *CFMutableArrayRef;

/** \defgroup CFArrayRef CFArray Reference
    \brief A CFArray and its mutable type, \ref CFMutableArrayRef
      "CFMutableArray", are simple, low overhead, ordered containers for
      objects.
    \details
      <code>\#include <CoreFoundation/CFArray.h></code>
    \{
 */

/** \name Callbacks
    \{
 */
typedef void (*CFArrayApplierFunction) (const void *value, void *context);
typedef CFStringRef (*CFArrayCopyDescriptionCallBack) (const void *value);
typedef void (*CFArrayReleaseCallBack) (CFAllocatorRef allocator,
                                        const void *value);
typedef const void *(*CFArrayRetainCallBack) (CFAllocatorRef allocator,
                                              const void *value);
typedef Boolean (*CFArrayEqualCallBack) (const void *value1,
                                         const void *value2);
/** \} */

/** \brief Structure with CFArray callbacks.
 */
typedef struct _CFArrayCallBacks CFArrayCallBacks;
struct _CFArrayCallBacks
{
  CFIndex version; /**< Structure's version number.  Current version is 0. */
  CFArrayRetainCallBack retain;
    /**< The callback used to retain values added to the array.  If NULL,
	 values are not retained. */
  CFArrayReleaseCallBack release;
  CFArrayCopyDescriptionCallBack copyDescription;
  CFArrayEqualCallBack equal;
};

/** \name Predefined Callback Structures
    \{
 */
CF_EXPORT const CFArrayCallBacks kCFTypeArrayCallBacks;
/** \} */



/** \name Creating an Array
    \{
 */
CF_EXPORT CFArrayRef
CFArrayCreate (CFAllocatorRef allocator, const void **values,
               CFIndex numValues, const CFArrayCallBacks * callBacks);

CF_EXPORT CFArrayRef
CFArrayCreateCopy (CFAllocatorRef allocator, CFArrayRef theArray);
/** \} */

/** \name Examining an Array
    \{
 */
CF_EXPORT CFIndex
CFArrayBSearchValues (CFArrayRef theArray, CFRange range, const void *value,
                      CFComparatorFunction comparator, void *context);

CF_EXPORT Boolean
CFArrayContainsValue (CFArrayRef theArray, CFRange range, const void *value);

CF_EXPORT CFIndex CFArrayGetCount (CFArrayRef theArray);

CF_EXPORT CFIndex
CFArrayGetCountOfValue (CFArrayRef theArray, CFRange range, const void *value);

CF_EXPORT CFIndex
CFArrayGetFirstIndexOfValue (CFArrayRef theArray, CFRange range,
                             const void *value);

CF_EXPORT CFIndex
CFArrayGetLastIndexOfValue (CFArrayRef theArray, CFRange range,
                            const void *value);

CF_EXPORT void
CFArrayGetValues (CFArrayRef theArray, CFRange range, const void **values);

CF_EXPORT const void *CFArrayGetValueAtIndex (CFArrayRef theArray, CFIndex idx);
/** \} */

/** \name Applying a Function to Elements
    \{
 */
CF_EXPORT void
CFArrayApplyFunction (CFArrayRef theArray, CFRange range,
                      CFArrayApplierFunction applier, void *context);
/** \} */

/** \name Getting the CFArray Type ID
    \{
 */
CF_EXPORT CFTypeID CFArrayGetTypeID (void);
/** \} */

/** \} */

/** \defgroup CFMutableArrayRef CFMutableArray Reference
    \details <code>\#include <CoreFoundation/CFArray.h></code>
    \{
 */
CF_EXPORT void
CFArrayAppendArray (CFMutableArrayRef theArray, CFArrayRef otherArray,
                    CFRange otherRange);

CF_EXPORT void
CFArrayAppendValue (CFMutableArrayRef theArray, const void *value);

CF_EXPORT CFMutableArrayRef
CFArrayCreateMutable (CFAllocatorRef allocator, CFIndex capacity,
                      const CFArrayCallBacks * callBacks);

CF_EXPORT CFMutableArrayRef
CFArrayCreateMutableCopy (CFAllocatorRef allocator, CFIndex capacity,
                          CFArrayRef theArray);

CF_EXPORT void
CFArrayExchangeValuesAtIndices (CFMutableArrayRef theArray, CFIndex idx1,
                                CFIndex idx2);

CF_EXPORT void
CFArrayInsertValueAtIndex (CFMutableArrayRef theArray, CFIndex idx,
                           const void *value);

CF_EXPORT void CFArrayRemoveAllValues (CFMutableArrayRef theArray);

CF_EXPORT void
CFArrayRemoveValueAtIndex (CFMutableArrayRef theArray, CFIndex idx);

CF_EXPORT void
CFArrayReplaceValues (CFMutableArrayRef theArray, CFRange range,
                      const void **newValues, CFIndex newCount);

CF_EXPORT void
CFArraySetValueAtIndex (CFMutableArrayRef theArray, CFIndex idx,
                        const void *value);

CF_EXPORT void
CFArraySortValues (CFMutableArrayRef theArray, CFRange range,
                   CFComparatorFunction comparator, void *context);

/** \} */

CF_EXTERN_C_END
#endif /* __COREFOUNDATION_CFARRAY_H__ */
