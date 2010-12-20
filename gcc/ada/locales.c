/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                             L O C A L E S                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *             Copyright (C) 2010, Free Software Foundation, Inc.           *
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

/*  This file provides OS-dependent support for the Ada.Locales package.    */

typedef char char4 [4];

/*
  c_get_language_code needs to fill in the Alpha-3 encoding of the
  language code (3 lowercase letters). That should be "und" if the
  language is unknown. [see Ada.Locales]
*/
void c_get_language_code (char4 p) {
  char *r = "und";
  for (; *r != '\0'; p++, r++)
    *p = *r;
}

/*
  c_get_country_code needs to fill in the Alpha-2 encoding of the
  country code (2 uppercase letters). That should be "ZZ" if the
  country is unknown. [see Ada.Locales]
*/
void c_get_country_code (char4 p) {
  char *r = "ZZ";
  for (; *r != '\0'; p++, r++)
    *p = *r;
}
