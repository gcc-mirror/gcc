/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              G A D A I N T                               *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *           Copyright (C) 2010-2023, Free Software Foundation, Inc.        *
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

/* This file contains the declarations of adaint.c material used in gigi.
   It should be used in lieu of adaint.h in gigi because the latter drags
   a lot of stuff on Windows and this pollutes the namespace of macros.  */

#ifndef GCC_ADAINT_H
#define GCC_ADAINT_H

#ifdef __cplusplus
extern "C" {
#endif

extern char *__gnat_to_canonical_file_spec (char *);

#ifdef __cplusplus
}
#endif

#endif /* GCC_ADAINT_H */
