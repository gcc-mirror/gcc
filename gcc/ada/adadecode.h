/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                            A D A D E C O D E                             *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *           Copyright (C) 2001-2006, Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 * As a  special  exception,  if you  link  this file  with other  files to *
 * produce an executable,  this file does not by itself cause the resulting *
 * executable to be covered by the GNU General Public License. This except- *
 * ion does not  however invalidate  any other reasons  why the  executable *
 * file might be covered by the  GNU Public License.                        *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This function will return the Ada name from the encoded form.
   The Ada coding is done in exp_dbug.ads and this is the inverse function.
   see exp_dbug.ads for full encoding rules, a short description is added
   below. Objects and routines are fully handled; types are stripped of their
   encodings.

   CODED_NAME is the encoded entity name.
   ADA_NAME is a pointer to a buffer, it will receive the Ada name. A safe
   size for this buffer is: strlen (coded_name) * 2 + 60. (60 is for the
   verbose information).
   VERBOSE is nonzero if more information about the entity is to be
   added at the end of the Ada name and surrounded by ( and ).  */
extern void __gnat_decode (const char *, char *, int);

/* This function will return the GNAT encodings, in a colon-separated list,
   from the encoded form. The Ada encodings are described in exp_dbug.ads.  */
extern void get_encoding (const char *, char *);

/* ada_demangle is added for COMPATIBILITY ONLY. It has the name of the
   function used in the binutils and GDB. Always consider using __gnat_decode
   instead of ada_demangle. Caller must free the pointer returned.  */
extern char *ada_demangle (const char *);
