/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                N A M E T                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 1992-2019, Free Software Foundation, Inc.       *
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

/* This is the C file that corresponds to the Ada package specification
   Namet.  It was created manually from files namet.ads and namet.adb.
   Subprograms from Exp_Dbug and Sinput are also made accessible here.  */

#ifdef __cplusplus
extern "C" {
#endif

/* Structure defining a names table entry.  */

struct Name_Entry
{
  Int Name_Chars_Index; /* Starting location of char in Name_Chars table. */
  Short Name_Len;         /* Length of this name in characters. */
  Byte Byte_Info;       /* Byte value associated with this name */
  Byte Spare;           /* Unused */
  Name_Id Hash_Link;    /* Link to next entry in names table for same hash
                           code. Not accessed by C routines.  */
  Int Int_Info;         /* Int value associated with this name */
};

/* Pointer to names table vector. */
#define Names_Ptr namet__name_entries__table
extern struct Name_Entry *Names_Ptr;

/* Pointer to name characters table. */
#define Name_Chars_Ptr namet__name_chars__table
extern char *Name_Chars_Ptr;

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

static char *Get_Name_String (Name_Id);

INLINE char *
Get_Name_String (Name_Id Id)
{
  return Name_Chars_Ptr + Names_Ptr[Id - First_Name_Id].Name_Chars_Index + 1;
}

#define Name_Equals namet__name_equals
extern Boolean Name_Equals (Name_Id, Name_Id);

/* The following routines and variables are not part of Namet, but we
   include the header here since it seems the best place for it.  */

#define Get_Encoded_Type_Name exp_dbug__get_encoded_type_name
extern Boolean Get_Encoded_Type_Name (Entity_Id);
#define Get_Variant_Encoding exp_dbug__get_variant_encoding
extern void Get_Variant_Encoding (Entity_Id);

#define Spec_Context_List exp_dbug__spec_context_list
#define Body_Context_List exp_dbug__body_context_list
extern char *Spec_Context_List, *Body_Context_List;
#define Spec_Filename exp_dbug__spec_filename
#define Body_Filename exp_dbug__body_filename
extern char *Spec_Filename, *Body_Filename;

/* Here are some functions in sinput.adb we call from trans.c.  */

typedef Nat Source_File_Index;
typedef Int Logical_Line_Number;
typedef Int Column_Number;

#define Debug_Source_Name sinput__debug_source_name
#define Full_Debug_Name sinput__full_debug_name
#define Reference_Name sinput__reference_name
#define Get_Source_File_Index sinput__get_source_file_index
#define Get_Logical_Line_Number sinput__get_logical_line_number
#define Get_Column_Number sinput__get_column_number
#define Instantiation sinput__instantiation

extern File_Name_Type Debug_Source_Name	(Source_File_Index);
extern File_Name_Type Full_Debug_Name	(Source_File_Index);
extern File_Name_Type Reference_Name	(Source_File_Index);
extern Source_File_Index Get_Source_File_Index (Source_Ptr);
extern Logical_Line_Number Get_Logical_Line_Number (Source_Ptr);
extern Column_Number Get_Column_Number (Source_Ptr);
extern Source_Ptr Instantiation (Source_File_Index);

#ifdef __cplusplus
}
#endif
