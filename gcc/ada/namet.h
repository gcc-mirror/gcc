/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                N A M E T                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 1992-2025, Free Software Foundation, Inc.       *
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

/* This is the C header that corresponds to the Ada package specification for
   Namet.  It also contains the implementation of inlined functions from the
   package body for Namet.  It was created manually from namet.ads and
   namet.adb and must be kept synchronized with changes in these files.  */

#ifdef __cplusplus
extern "C" {
#endif

/* Structure defining a name table entry.  */
struct Name_Entry
{
  Int Name_Chars_Index;
  Short Name_Len;
  Byte Byte_Info;
  Byte Name_Has_No_Encodings : 1;
  Byte Boolean1_Info : 1;
  Byte Boolean2_Info : 1;
  Byte Boolean3_Info : 1;
  Byte Spare : 4;
  Name_Id Hash_Link;
  Int Int_Info;
};

/* Pointer to the name table.  */
#define Names_Ptr namet__name_entries__table
extern struct Name_Entry (*Names_Ptr)[];

/* Pointer to the name character table.  */
#define Name_Chars_Ptr namet__name_chars__table
extern char (*Name_Chars_Ptr)[];

/* This is Hostparm.Max_Line_Length.  */
#define Max_Line_Length (32767 - 1)

/* The global name buffer.  */
struct Bounded_String
{
  Nat Max_Length;
  Nat Length;
  char Chars[4 * Max_Line_Length]; /* Exact value for overflow detection.  */
};

#define Global_Name_Buffer namet__global_name_buffer
extern struct Bounded_String Global_Name_Buffer;

#define Name_Buffer Global_Name_Buffer.Chars
#define Name_Len Global_Name_Buffer.Length

/* Get_Name_String returns a NUL terminated C string for the specified name.
   We could use the official Ada routine for this purpose, but since the
   strings we want are sitting in the name strings table in exactly the form
   we need them (NUL terminated), we just point to the name directly. */

INLINE char *Get_Name_String (Name_Id);

INLINE char *
Get_Name_String (Name_Id Id)
{
  return
    &(*Name_Chars_Ptr)[(*Names_Ptr)[Id - First_Name_Id].Name_Chars_Index + 1];
}

#define Name_Equals namet__name_equals
extern Boolean Name_Equals (Name_Id, Name_Id);

#ifdef __cplusplus
}
#endif
