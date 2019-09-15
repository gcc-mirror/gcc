/* CFAvailability.h
   
   Copyright (C) 2017 Free Software Foundation, Inc.
   
   Written by: Stefan Bidigaray
   Date: August, 2017
   
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


#ifndef __COREFOUNDATION_CFAVAILABILITY_H__
#define __COREFOUNDATION_CFAVAILABILITY_H__

/* Compiler features */
#ifndef __has_feature
#define __has_feature(x) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif
#ifndef __has_extension
#define __has_extension(x) __has_feature
#endif

/* CFEnum macro for type definitions */
#if (__cplusplus && __cplusplus >= 201103L)
#define CF_ENUM(_type, _name) _type _name; enum : _type
#define CF_OPTIONS(_type, _name) _type _name; enum : _type
#else
#define CF_ENUM(_type, _name) _type _name; enum
#define CF_OPTIONS(_type, _name) _type _name; enum
#endif

#endif /* __COREFOUNDATION_CFAVAILABILITY_H__ */

