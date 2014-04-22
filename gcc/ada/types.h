/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                T Y P E S                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2013, Free Software Foundation, Inc.         *
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

/* This is the C file that corresponds to the Ada package spec Types. It was
   created manually from the files types.ads and types.adb.

   This package contains host independent type definitions which are used
   throughout the compiler modules. The comments in the C version are brief
   reminders of the purpose of each declaration.  For complete documentation,
   see the Ada version of these definitions.  */

/* Boolean Types:  */

/* Boolean type (cannot use enum, because of bit field restriction on some
   compilers).  */
typedef unsigned char Boolean;
#define False 0
#define True  1

/* General Use Integer Types */

/* Signed 32-bit integer */
typedef int Int;

/* Signed 16-bit integer */
typedef short Short;

/* Non-negative Int values */
typedef Int Nat;

/* Positive Int values */
typedef Int Pos;

/* 8-bit unsigned integer */
typedef unsigned char Byte;

/* 8-Bit Character and String Types:  */

/* 8-bit character type */
typedef char Char;

/* Graphic characters, as defined in ARM */
typedef Char Graphic_Character;

/* Line terminator characters (LF, VT, FF, CR) */
typedef Char Line_Terminator;

/* Characters with the upper bit set */
typedef Char Upper_Half_Character;

/* String type built on Char (note that zero is an OK index) */
typedef Char *Str;

/* Pointer to string of Chars */
typedef Char *Str_Ptr;

/* Types for the fat pointer used for strings and the template it points to.
   On most platforms the fat pointer is naturally aligned but, on the rest,
   it is given twice the natural alignment.  For maximum portability, we do
   not overalign the type but only the objects.  */
typedef struct { int Low_Bound, High_Bound; } String_Template;
typedef struct { const char *Array; String_Template *Bounds; } String_Pointer;
#define DECLARE_STRING_POINTER(...) \
  __attribute__ ((aligned (sizeof (char *) * 2))) String_Pointer __VA_ARGS__

/* Types for Node/Entity Kinds:  */

/* The reason that these are defined here in the C version, rather than in the
   corresponding packages is that the requirement for putting bodies of
   inlined stuff IN the C header changes the dependencies.  Both sinfo.h
   and einfo.h now reference routines defined in tree.h.

   Note: these types would more naturally be defined as unsigned  char, but
   once again, the annoying restriction on bit fields for some compilers
   bites us!  */

typedef unsigned int Node_Kind;
typedef unsigned int Entity_Kind;

/* Types used for Text Buffer Handling:  */

/* Type used for subscripts in text buffer.  */
typedef Int Text_Ptr;

/* Text buffer used to hold source file or library information file.  */
typedef Char *Text_Buffer;

/* Pointer to text buffer.  */
typedef Char *Text_Buffer_Ptr;

/* Types used for Source Input Handling:  */

/* Line number type, used for storing all line numbers.  */
typedef Int Line_Number_Type;

/* Column number type, used for storing all column numbers.  */
typedef Int Column_Number_Type;

/* Type used to store text of a source file.  */
typedef Text_Buffer Source_Buffer;

/* Pointer to source buffer. */
typedef Text_Buffer_Ptr Source_Buffer_Ptr;

/* Type used for source location.  */
typedef Text_Ptr Source_Ptr;

/* Value used to indicate no source position set.  */
#define No_Location -1

/* Used for Sloc in all nodes in the representation of package Standard.  */
#define Standard_Location -2

/* Instance identifiers */
typedef Nat Instance_Id;

/* Type used for union of all possible ID values covering all ranges */
typedef int Union_Id;

/* Range definitions for Tree Data:  */

#define List_Low_Bound		-100000000
#define List_High_Bound		0

#define Node_Low_Bound		0
#define Node_High_Bound		99999999

#define Elist_Low_Bound		100000000
#define Elist_High_Bound	199999999

#define Elmt_Low_Bound		200000000
#define Elmt_High_Bound		299999999

#define Names_Low_Bound		300000000
#define Names_High_Bound	399999999

#define Strings_Low_Bound	400000000
#define Strings_High_Bound	499999999

#define Ureal_Low_Bound		500000000
#define Ureal_High_Bound        599999999

#define Uint_Low_Bound		600000000
#define Uint_Table_Start        2000000000
#define Uint_High_Bound	        2099999999

SUBTYPE (List_Range,      Int, List_Low_Bound,    List_High_Bound)
SUBTYPE (Node_Range,      Int, Node_Low_Bound,    Node_High_Bound)
SUBTYPE (Elist_Range,     Int, Elist_Low_Bound,   Elist_High_Bound)
SUBTYPE (Elmt_Range,      Int, Elmt_Low_Bound,    Elmt_High_Bound)
SUBTYPE (Names_Range,     Int, Names_Low_Bound,   Names_High_Bound)
SUBTYPE (Strings_Range,   Int, Strings_Low_Bound, Strings_High_Bound)
SUBTYPE (Uint_Range,      Int, Uint_Low_Bound,    Uint_High_Bound)
SUBTYPE (Ureal_Range,     Int, Ureal_Low_Bound,   Ureal_High_Bound)

/* Types for Names_Table Package:  */

typedef Int Name_Id;

/* Name_Id value for no name present.  */
#define No_Name Names_Low_Bound

/* Name_Id value for bad name.  */
#define Error_Name (Names_Low_Bound + 1)

/* First subscript of names table. */
#define First_Name_Id (Names_Low_Bound + 2)

/* Types for Tree Package:  */

/* Subscript of nodes table entry.  */
typedef Int Node_Id;

/* Used in semantics for Node_Id value referencing an entity.  */
typedef Node_Id Entity_Id;

/* Null node.  */
#define Empty 0

/* Error node.  */
#define Error 1

/* Subscript of first allocated node.  */
#define First_Node_Id Empty

/* Subscript of entry in lists table.  */
typedef Int List_Id;

/* Indicates absence of a list.  */
#define No_List 0

/* Error list. */
#define Error_List List_Low_Bound

/* Subscript of first allocated list header.  */
#define First_List_Id Error_List

/* Element list Id, subscript value of entry in lists table.  */
typedef Int Elist_Id;

/* Used to indicate absence of an element list.  */
#define No_Elist Elist_Low_Bound

/* Subscript of first allocated elist header */
#define First_Elist_Id (No_Elist + 1)

/* Element Id, subscript value of entry in elements table.  */
typedef Int Elmt_Id;

/* Used to indicate absence of a list element.  */
#define No_Elmt Elmt_Low_Bound

/* Subscript of first allocated element */
#define First_Elmt_Id (No_Elmt + 1)

/* Types for String_Table Package:  */

/* Subscript of strings table entry.  */
typedef Int String_Id;

/* Used to indicate missing string Id.  */
#define No_String Strings_Low_Bound

/* Subscript of first entry in strings table.  */
#define First_String_Id (No_String + 1)

/* Types for Uint_Support Package:  */

/* Type used for representation of universal integers.  */
typedef Int Uint;

/* Used to indicate missing Uint value.  */
#define No_Uint Uint_Low_Bound

/* Base value used to represent Uint values.  */
#define Base 32768

/* Minimum and maximum integers directly representable as Uint values */
#define Min_Direct (-(Base - 1))
#define Max_Direct ((Base - 1) * (Base - 1))

#define Uint_Direct_Bias  (Uint_Low_Bound + Base)
#define Uint_Direct_First (Uint_Direct_Bias + Min_Direct)
#define Uint_Direct_Last  (Uint_Direct_Bias + Max_Direct)

/* Define range of direct biased values */
SUBTYPE (Uint_Direct, Uint, Uint_Direct_First, Uint_Direct_Last)

/* Constants in Uint format.  */
#define Uint_0  (Uint_Direct_Bias + 0)
#define Uint_1  (Uint_Direct_Bias + 1)
#define Uint_2  (Uint_Direct_Bias + 2)
#define Uint_10 (Uint_Direct_Bias + 10)
#define Uint_16 (Uint_Direct_Bias + 16)

#define Uint_Minus_1 (Uint_Direct_Bias - 1)

/* Types for Ureal_Support Package:  */

/* Type used for representation of universal reals.  */
typedef Int Ureal;

/* Used to indicate missing Uint value.  */
#define No_Ureal Ureal_Low_Bound

/* Subscript of first entry in Ureal table.  */
#define Ureal_First_Entry (No_Ureal + 1)

/* Character Code Type:  */

/* Character code value, intended to be 32 bits.  */
typedef unsigned Char_Code;

/* Types Used for Library Management:  */

/* Unit number.  */
typedef Int Unit_Number_Type;

/* Unit number value for main unit.  */
#define Main_Unit 0

/* Type used for lines table.  */
typedef Source_Ptr *Lines_Table_Type;

/* Type used for pointer to lines table.  */
typedef Source_Ptr *Lines_Table_Ptr;

/* Length of time stamp value.  */
#define Time_Stamp_Length 22

/* Type used to represent time stamp.  */
typedef Char *Time_Stamp_Type;

/* Name_Id synonym used for file names.  */
typedef Name_Id File_Name_Type;

/* Constant used to indicate no file found.  */
#define No_File No_Name

/* Name_Id synonym used for unit names.  */
typedef Name_Id Unit_Name_Type;

/* Definitions for mechanism type and values */
typedef Int Mechanism_Type;
#define Default            0
#define By_Copy            (-1)
#define By_Reference       (-2)
#define By_Descriptor      (-3)
#define By_Descriptor_UBS  (-4)
#define By_Descriptor_UBSB (-5)
#define By_Descriptor_UBA  (-6)
#define By_Descriptor_S    (-7)
#define By_Descriptor_SB   (-8)
#define By_Descriptor_A    (-9)
#define By_Descriptor_NCA  (-10)
#define By_Descriptor_Last (-10)
#define By_Short_Descriptor      (-11)
#define By_Short_Descriptor_UBS  (-12)
#define By_Short_Descriptor_UBSB (-13)
#define By_Short_Descriptor_UBA  (-14)
#define By_Short_Descriptor_S    (-15)
#define By_Short_Descriptor_SB   (-16)
#define By_Short_Descriptor_A    (-17)
#define By_Short_Descriptor_NCA  (-18)
#define By_Short_Descriptor_Last (-18)

/* Internal to Gigi.  */
#define By_Copy_Return     (-128)

/* Definitions of Reason codes for Raise_xxx_Error nodes */
#define CE_Access_Check_Failed              0
#define CE_Access_Parameter_Is_Null         1
#define CE_Discriminant_Check_Failed        2
#define CE_Divide_By_Zero                   3
#define CE_Explicit_Raise                   4
#define CE_Index_Check_Failed               5
#define CE_Invalid_Data                     6
#define CE_Length_Check_Failed              7
#define CE_Null_Exception_Id                8
#define CE_Null_Not_Allowed                 9
#define CE_Overflow_Check_Failed           10
#define CE_Partition_Check_Failed          11
#define CE_Range_Check_Failed              12
#define CE_Tag_Check_Failed                13

#define PE_Access_Before_Elaboration       14
#define PE_Accessibility_Check_Failed      15
#define PE_Address_Of_Intrinsic            16
#define PE_Aliased_Parameters              17
#define PE_All_Guards_Closed               18
#define PE_Bad_Attribute_For_Predicate     19
#define PE_Current_Task_In_Entry_Body      20
#define PE_Duplicated_Entry_Address        21
#define PE_Explicit_Raise                  22
#define PE_Finalize_Raised_Exception       23
#define PE_Implicit_Return                 24
#define PE_Misaligned_Address_Value        25
#define PE_Missing_Return                  26
#define PE_Overlaid_Controlled_Object      27
#define PE_Potentially_Blocking_Operation  28
#define PE_Stubbed_Subprogram_Called       29
#define PE_Unchecked_Union_Restriction     30
#define PE_Non_Transportable_Actual        31

#define SE_Empty_Storage_Pool              32
#define SE_Explicit_Raise                  33
#define SE_Infinite_Recursion              34
#define SE_Object_Too_Large                35

#define LAST_REASON_CODE                   35
