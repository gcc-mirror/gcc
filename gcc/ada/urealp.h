/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               U R E A L P                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *                            $Revision: 1.2 $
 *                                                                          *
 *          Copyright (C) 1992-2001 Free Software Foundation, Inc.          *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file corresponds to the Ada package specification Urealp. It was
   created manually from the files urealp.ads and urealp.adb  */

/* Support for universal real arithmetic.  */

#define Numerator urealp__numerator
extern Uint Numerator		PARAMS ((Ureal));

#define Denominator urealp__denominator
extern Uint Denominator		PARAMS ((Ureal));

#define Rbase urealp__rbase
extern Nat Rbase		PARAMS ((Ureal));

#define UR_Is_Negative urealp__ur_is_negative
extern Boolean UR_Is_Negative	PARAMS ((Ureal));

#define UR_Is_Zero urealp__ur_is_zero
extern Boolean UR_Is_Zero	PARAMS ((Ureal));

enum Rounding_Mode {Floor = 0, Ceiling = 1, Round = 2, Round_Even = 3};

#define Machine eval_fat__machine
extern Ureal Machine		PARAMS ((Entity_Id, Ureal,
					 enum Rounding_Mode));
