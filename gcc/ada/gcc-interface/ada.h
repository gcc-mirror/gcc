/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                  A D A                                   *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2009, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed  with GNAT;  see file  COPYING3.  If not see *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file contains some standard macros for performing Ada-like
   operations. These are used to aid in the translation of other headers. */

#ifndef GCC_ADA_H
#define GCC_ADA_H

/* Inlined functions in header are preceded by INLINE, which is normally set
   to extern inline for GCC, but may be set to static for use in standard
   ANSI-C.  */

#ifndef INLINE
#ifdef __GNUC__
#define INLINE static inline
#else
#define INLINE static
#endif
#endif

/* Define a macro to concatenate two strings.  Write it for ANSI C and
   for traditional C.  */

#ifdef __STDC__
#define CAT(A,B) A##B
#else
#define _ECHO(A) A
#define CAT(A,B) ECHO(A)B
#endif

/* The following macro definition simulates the effect of a declaration of
   a subtype, where the first two parameters give the name of the type and
   subtype, and the third and fourth parameters give the subtype range. The
   effect is to compile a typedef defining the subtype as a synonym for the
   type, together with two constants defining the end points.  */

#define SUBTYPE(SUBTYPE,TYPE,FIRST,LAST)	\
  typedef TYPE SUBTYPE;				\
  enum { CAT (SUBTYPE,__First) = FIRST,		\
         CAT (SUBTYPE,__Last) = LAST };

/* The following definition provides the equivalent of the Ada IN operator,
   assuming that the subtype involved has been defined using the SUBTYPE
   macro defined above.  */

#define IN(VALUE,SUBTYPE) \
  (((VALUE) >= (SUBTYPE) CAT (SUBTYPE,__First)) \
   && ((VALUE) <= (SUBTYPE) CAT (SUBTYPE,__Last)))

#endif
