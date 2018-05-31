/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                          V X _ C R T B E G I N                           *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *              Copyright (C) 2016-2018, Free Software Foundation, Inc.     *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* crtbegin kind of file for ehframe registration/deregistration
   purposes on VxWorks.  This variant exposes the ctor/dtor functions
   as explicit constructors referenced from a .ctors/.dtors section.  */

#define CTOR_ATTRIBUTE
#define DTOR_ATTRIBUTE

#include "vx_crtbegin.inc"

/* 101 is the highest user level priority allowed by VxWorks.  */

static void (* volatile eh_registration_ctors[])()
  __attribute__((section (".ctors.65424")))
= { &CTOR_NAME };

static void (* volatile eh_registration_dtors[])()
  __attribute__((section (".dtors.65424")))
= { &DTOR_NAME };
