/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                N A M E T                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2005 Free Software Foundation, Inc.          *
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
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C file that corresponds to the Ada package specification
   Namet. It was created manually from files namet.ads and namet.adb.  */

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

#define Name_Buffer namet__name_buffer
extern char Name_Buffer[];

extern Int namet__name_len;
#define Name_Len namet__name_len

/* Get_Name_String returns a null terminated C string for the specified name.
   We could use the official Ada routine for this purpose, but since the
   strings we want are sitting in the name strings table in exactly the form
   we need them (null terminated), we just point to the name directly. */

static char *Get_Name_String (Name_Id);

INLINE char *
Get_Name_String (Name_Id Id)
{
  return Name_Chars_Ptr + Names_Ptr[Id - First_Name_Id].Name_Chars_Index + 1;
}

/* Get_Decoded_Name_String returns a null terminated C string in the same
   manner as Get_Name_String, except that it is decoded (i.e. upper half or
   wide characters are put back in their external form, and character literals
   are also returned in their external form (with surrounding apostrophes) */

extern void namet__get_decoded_name_string (Name_Id);

static char *Get_Decoded_Name_String (Name_Id);

INLINE char *
Get_Decoded_Name_String (Name_Id Id)
{
  namet__get_decoded_name_string (Id);
  Name_Buffer[Name_Len] = 0;
  return Name_Buffer;
}

/* Like Get_Decoded_Name_String, but the result has all qualification and
   package body entity suffixes stripped, and also all letters are upper
   cased.  This is used fo rbuilding the enumeration literal table. */

extern void casing__set_all_upper_case (void);

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

#define Is_Non_Ada_Error exp_ch11__is_non_ada_error
extern Boolean Is_Non_Ada_Error (Entity_Id);

/* Here are some functions in sinput.adb we call from a-trans.c.  */
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
