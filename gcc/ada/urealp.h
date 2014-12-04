/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               U R E A L P                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 1992-2014, Free Software Foundation, Inc.       *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT; see file COPYING3.  If not, go to *
 * http://www.gnu.org/licenses for a complete copy of the license.          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file corresponds to the Ada package specification Urealp.  It was
   created manually from the files urealp.ads and urealp.adb.  */

#ifdef __cplusplus
extern "C" {
#endif

/* Support for universal real arithmetic.  */

#define Numerator urealp__numerator
extern Uint Numerator		(Ureal);

#define Denominator urealp__denominator
extern Uint Denominator		(Ureal);

#define Rbase urealp__rbase
extern Nat Rbase		(Ureal);

#define Norm_Den urealp__norm_den
extern Uint Norm_Den		(Ureal);

#define Norm_Num urealp__norm_num
extern Uint Norm_Num		(Ureal);

#define UR_Is_Negative urealp__ur_is_negative
extern Boolean UR_Is_Negative	(Ureal);

#define UR_Is_Zero urealp__ur_is_zero
extern Boolean UR_Is_Zero	(Ureal);

enum Rounding_Mode {Floor = 0, Ceiling = 1, Round = 2, Round_Even = 3};

#define Machine eval_fat__machine
extern Ureal Machine		(Entity_Id, Ureal, enum Rounding_Mode,
				 Node_Id);

#ifdef __cplusplus
}
#endif
