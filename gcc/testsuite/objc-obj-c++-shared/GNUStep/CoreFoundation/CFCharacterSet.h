/* CFCharacterSet.h
   
   Copyright (C) 2012 Free Software Foundation, Inc.
   
   Written by: Stefan Bidigaray
   Date: January, 2012
   
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

#ifndef __COREFOUNDATION_CFCHARACTERSET_H__
#define __COREFOUNDATION_CFCHARACTERSET_H__

#include "CFBase.h"
#include "CFData.h"

CF_EXTERN_C_BEGIN

/** \ingroup CFCharacterSetRef */
typedef const struct __CFCharacterSet * CFCharacterSetRef;
/** \ingroup CFMutableCharacterSetRef */
typedef struct __CFCharacterSet * CFMutableCharacterSetRef;

/** \defgroup CFCharacterSetRef CFCharacterSet Reference
    \{
 */
typedef enum
{
  kCFCharacterSetControl = 1,
  kCFCharacterSetWhitespace,
  kCFCharacterSetWhitespaceAndNewline,
  kCFCharacterSetDecimalDigit,
  kCFCharacterSetLetter,
  kCFCharacterSetLowercaseLetter,
  kCFCharacterSetUppercaseLetter,
  kCFCharacterSetNonBase,
  kCFCharacterSetDecomposable,
  kCFCharacterSetAlphaNumeric,
  kCFCharacterSetPunctuation,
#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
  kCFCharacterSetCapitalizedLetter = 13,
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_3, GS_API_LATEST)
  kCFCharacterSetSymbol = 14,
#endif
#if OS_API_VERSION(MAC_OS_X_VERSION_10_5, GS_API_LATEST)
  kCFCharacterSetNewline = 15,
#endif
  kCFCharacterSetIllegal = 12
} CFCharacterSetPredefinedSet;



/** \name Creating Character Sets
    \{
 */
#if OS_API_VERSION(MAC_OS_X_VERSION_10_3, GS_API_LATEST)
CF_EXPORT CFCharacterSetRef
CFCharacterSetCreateCopy (CFAllocatorRef alloc, CFCharacterSetRef set);
#endif

#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
CF_EXPORT CFCharacterSetRef
CFCharacterSetCreateInvertedSet (CFAllocatorRef alloc, CFCharacterSetRef set);
#endif

CF_EXPORT CFCharacterSetRef
CFCharacterSetCreateWithCharactersInRange (CFAllocatorRef alloc,
  CFRange range);

CF_EXPORT CFCharacterSetRef
CFCharacterSetCreateWithCharactersInString (CFAllocatorRef alloc,
  CFStringRef string);

CF_EXPORT CFCharacterSetRef
CFCharacterSetCreateWithBitmapRepresentation (CFAllocatorRef alloc,
  CFDataRef data);
/** \} */

/** \name Getting Predefined Character Sets
    \{
 */
CF_EXPORT CFCharacterSetRef
CFCharacterSetGetPredefined (CFCharacterSetPredefinedSet setIdentifier);
/** \} */

/** \name Querying Character Sets
    \{
 */
CF_EXPORT CFDataRef
CFCharacterSetCreateBitmapRepresentation (CFAllocatorRef alloc,
  CFCharacterSetRef set);

CF_EXPORT Boolean
CFCharacterSetIsCharacterMember (CFCharacterSetRef set, UniChar c);

#if OS_API_VERSION(MAC_OS_X_VERSION_10_2, GS_API_LATEST)
CF_EXPORT Boolean
CFCharacterSetHasMemberInPlane (CFCharacterSetRef set, CFIndex plane);

CF_EXPORT Boolean
CFCharacterSetIsLongCharacterMember (CFCharacterSetRef set, UTF32Char c);

CF_EXPORT Boolean
CFCharacterSetIsSupersetOfSet (CFCharacterSetRef set,
  CFCharacterSetRef otherSet);
#endif
/** \} */

/** \name Getting the Character Set Type Identifier
    \{
 */
CF_EXPORT CFTypeID
CFCharacterSetGetTypeID (void);
/** \} */
/** \} */

/** \defgroup CFMutableCharacterSetRef CFMutableCharacterSet Reference
    \{
 */
CF_EXPORT CFMutableCharacterSetRef
CFCharacterSetCreateMutable (CFAllocatorRef alloc);

CF_EXPORT CFMutableCharacterSetRef
CFCharacterSetCreateMutableCopy (CFAllocatorRef alloc, CFCharacterSetRef set);

CF_EXPORT void
CFCharacterSetAddCharactersInRange (CFMutableCharacterSetRef set,
  CFRange range);

CF_EXPORT void
CFCharacterSetAddCharactersInString (CFMutableCharacterSetRef set,
  CFStringRef string);

CF_EXPORT void
CFCharacterSetRemoveCharactersInRange (CFMutableCharacterSetRef set,
  CFRange range);

CF_EXPORT void
CFCharacterSetRemoveCharactersInString (CFMutableCharacterSetRef set,
  CFStringRef string);

CF_EXPORT void
CFCharacterSetIntersect (CFMutableCharacterSetRef set,
  CFCharacterSetRef otherSet);

CF_EXPORT void
CFCharacterSetInvert (CFMutableCharacterSetRef set);

CF_EXPORT void
CFCharacterSetUnion (CFMutableCharacterSetRef set, CFCharacterSetRef otherSet);
/** \} */

CF_EXTERN_C_END

#endif /* __COREFOUNDATION_CFCHARACTERSET_H__ */

