/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S N A M E S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *                                                                          *
 *          Copyright (C) 1992-2002 Free Software Foundation, Inc.          *
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

/* This is the C file that corresponds to the Ada package specification
   Snames. It was created manually from the file snames.ads. */

/* Name_Id values */

#define Name_uParent    (First_Name_Id + 256 + 0)
#define Name_uTag       (First_Name_Id + 256 + 1)
#define Name_Off        (First_Name_Id + 256 + 2)
#define Name_Space		(First_Name_Id + 256 + 3)
#define Name_Time       (First_Name_Id + 256 + 4)
#define Name_uInit_Proc (First_Name_Id + 256 + 5)
#define Name_uSize      (First_Name_Id + 256 + 6)

/* Define the function to return one of the numeric values below. Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Attribute_Id snames__get_attribute_id
extern char Get_Attribute_Id PARAMS ((int));

/* Define the numeric values for the attributes.  */

#define  Attr_Abort_Signal                   0
#define  Attr_Access                         1
#define  Attr_Address                        2
#define  Attr_Address_Size                   3
#define  Attr_Aft                            4
#define  Attr_Alignment                      5
#define  Attr_Asm_Input                      6
#define  Attr_Asm_Output                     7
#define  Attr_AST_Entry                      8
#define  Attr_Bit                            9
#define  Attr_Bit_Order                     10
#define  Attr_Bit_Position                  11
#define  Attr_Body_Version                  12
#define  Attr_Callable                      13
#define  Attr_Caller                        14
#define  Attr_Code_Address                  15
#define  Attr_Component_Size                16
#define  Attr_Compose                       17
#define  Attr_Constrained                   18
#define  Attr_Count                         19
#define  Attr_Default_Bit_Order             20
#define  Attr_Definite                      21
#define  Attr_Delta                         22
#define  Attr_Denorm                        23
#define  Attr_Digits                        24
#define  Attr_Elaborated                    25
#define  Attr_Emax                          26
#define  Attr_Enum_Rep                      27
#define  Attr_Epsilon                       28
#define  Attr_Exponent                      29
#define  Attr_External_Tag                  30
#define  Attr_First                         31
#define  Attr_First_Bit                     32
#define  Attr_Fixed_Value                   33
#define  Attr_Fore                          34
#define  Attr_Has_Discriminants             35
#define  Attr_Identity                      36
#define  Attr_Img                           37
#define  Attr_Integer_Value                 38
#define  Attr_Large                         39
#define  Attr_Last                          40
#define  Attr_Last_Bit                      41
#define  Attr_Leading_Part                  42
#define  Attr_Length                        43
#define  Attr_Machine_Emax                  44
#define  Attr_Machine_Emin                  45
#define  Attr_Machine_Mantissa              46
#define  Attr_Machine_Overflows             47
#define  Attr_Machine_Radix                 48
#define  Attr_Machine_Rounds                49
#define  Attr_Machine_Size                  50
#define  Attr_Mantissa                      51
#define  Attr_Max_Size_In_Storage_Elements  52
#define  Attr_Maximum_Alignment             53
#define  Attr_Mechanism_Code                54
#define  Attr_Model_Emin                    55
#define  Attr_Model_Epsilon                 56
#define  Attr_Model_Mantissa                57
#define  Attr_Model_Small                   58
#define  Attr_Modulus                       59
#define  Attr_Null_Parameter                60
#define  Attr_Object_Size                   61
#define  Attr_Partition_ID                  62
#define  Attr_Passed_By_Reference           63
#define  Attr_Pos                           64
#define  Attr_Position                      65
#define  Attr_Range                         66
#define  Attr_Range_Length                  67
#define  Attr_Round                         68
#define  Attr_Safe_Emax                     69
#define  Attr_Safe_First                    70
#define  Attr_Safe_Large                    71
#define  Attr_Safe_Last                     72
#define  Attr_Safe_Small                    73
#define  Attr_Scale                         74
#define  Attr_Scaling                       75
#define  Attr_Signed_Zeros                  76
#define  Attr_Size                          77
#define  Attr_Small                         78
#define  Attr_Storage_Size                  79
#define  Attr_Storage_Unit                  80
#define  Attr_Tag                           81
#define  Attr_Terminated                    82
#define  Attr_To_Address                    83
#define  Attr_Type_Class                    84
#define  Attr_UET_Address                   85
#define  Attr_Unbiased_Rounding             86
#define  Attr_Unchecked_Access              87
#define  Attr_Universal_Literal_String      88
#define  Attr_Unrestricted_Access           89
#define  Attr_VADS_Size                     90
#define  Attr_Val                           91
#define  Attr_Valid                         92
#define  Attr_Value_Size                    93
#define  Attr_Version                       94
#define  Attr_Wide_Character_Size           95
#define  Attr_Wide_Width                    96
#define  Attr_Width                         97
#define  Attr_Word_Size                     98

#define  Attr_Adjacent                      99
#define  Attr_Ceiling                      100
#define  Attr_Copy_Sign                    101
#define  Attr_Floor                        102
#define  Attr_Fraction                     103
#define  Attr_Image                        104
#define  Attr_Input                        105
#define  Attr_Machine                      106
#define  Attr_Max                          107
#define  Attr_Min                          108
#define  Attr_Model                        109
#define  Attr_Pred                         110
#define  Attr_Remainder                    111
#define  Attr_Rounding                     112
#define  Attr_Succ                         113
#define  Attr_Truncation                   114
#define  Attr_Value                        115
#define  Attr_Wide_Image                   116
#define  Attr_Wide_Value                   117

#define  Attr_Output                       118
#define  Attr_Read                         119
#define  Attr_Write                        120

#define  Attr_Elab_Body                    121
#define  Attr_Elab_Spec                    122
#define  Attr_Storage_Pool                 123

#define  Attr_Base                         124
#define  Attr_Class                        125

/* Define the function to check if a Name_Id value is a valid pragma */

#define Is_Pragma_Name snames__is_pragma_name
extern Boolean Is_Pragma_Name PARAMS ((Name_Id));

/* Define the function to return one of the numeric values below.  Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Pragma_Id snames__get_pragma_id
extern char Get_Pragma_Id PARAMS ((int));

/* Define the numeric values for the pragmas. */

/* Configuration pragmas first */

#define  Pragma_Ada_83                       0
#define  Pragma_Ada_95                       1
#define  Pragma_C_Pass_By_Copy               2
#define  Pragma_Component_Alignment          3
#define  Pragma_Convention_Identifier        4
#define  Pragma_Discard_Names                5
#define  Pragma_Elaboration_Checking         6
#define  Pragma_Eliminate                    7
#define  Pragma_Extend_System                8
#define  Pragma_Extensions_Allowed           9
#define  Pragma_External_Name_Casing        10
#define  Pragma_Float_Representation        11
#define  Pragma_Initialize                  12
#define  Pragma_License                     13
#define  Pragma_Locking_Policy              14
#define  Pragma_Long_Float                  15
#define  Pragma_No_Run_Time                 16
#define  Pragma_Normalize_Scalars           17
#define  Pragma_Polling                     18
#define  Pragma_Propagate_Exceptions        19
#define  Pragma_Queuing_Policy              20
#define  Pragma_Ravenscar                   21
#define  Pragma_Restricted_Run_Time         22
#define  Pragma_Restrictions                23
#define  Pragma_Reviewable                  24
#define  Pragma_Source_File_Name            25
#define  Pragma_Style_Checks                26
#define  Pragma_Suppress                    27
#define  Pragma_Task_Dispatching_Policy     28
#define  Pragma_Unsuppress                  29
#define  Pragma_Use_VADS_Size               30
#define  Pragma_Validity_Checks             31
#define  Pragma_Warnings                    32

/* Remaining pragmas */

#define  Pragma_Abort_Defer                 33
#define  Pragma_All_Calls_Remote            34
#define  Pragma_Annotate                    35
#define  Pragma_Assert                      36
#define  Pragma_Asynchronous                37
#define  Pragma_Atomic                      38
#define  Pragma_Atomic_Components           39
#define  Pragma_Attach_Handler              40
#define  Pragma_Comment                     41
#define  Pragma_Common_Object               42
#define  Pragma_Complex_Representation      43
#define  Pragma_Controlled                  44
#define  Pragma_Convention                  45
#define  Pragma_CPP_Class                   46
#define  Pragma_CPP_Constructor             47
#define  Pragma_CPP_Virtual                 48
#define  Pragma_CPP_Vtable                  49
#define  Pragma_Debug                       50
#define  Pragma_Elaborate                   51
#define  Pragma_Elaborate_All               52
#define  Pragma_Elaborate_Body              53
#define  Pragma_Export                      54
#define  Pragma_Export_Exception            55
#define  Pragma_Export_Function             56
#define  Pragma_Export_Object               57
#define  Pragma_Export_Procedure            58
#define  Pragma_Export_Valued_Procedure     59
#define  Pragma_External                    60
#define  Pragma_Finalize_Storage_Only       61
#define  Pragma_Ident                       62
#define  Pragma_Import                      63
#define  Pragma_Import_Exception            64
#define  Pragma_Import_Function             65
#define  Pragma_Import_Object               66
#define  Pragma_Import_Procedure            67
#define  Pragma_Import_Valued_Procedure     68
#define  Pragma_Inline                      69
#define  Pragma_Inline_Always               70
#define  Pragma_Inline_Generic              71
#define  Pragma_Inspection_Point            72
#define  Pragma_Interface                   73
#define  Pragma_Interface_Name              74
#define  Pragma_Interrupt_Handler           75
#define  Pragma_Interrupt_Priority          76
#define  Pragma_Java_Constructor            77
#define  Pragma_Java_Interface              78
#define  Pragma_Link_With                   79
#define  Pragma_Linker_Alias                80
#define  Pragma_Linker_Options              81
#define  Pragma_Linker_Section              82
#define  Pragma_List                        83
#define  Pragma_Machine_Attribute           84
#define  Pragma_Main                        85
#define  Pragma_Main_Storage                86
#define  Pragma_Memory_Size                 87
#define  Pragma_No_Return                   88
#define  Pragma_Optimize                    89
#define  Pragma_Pack                        90
#define  Pragma_Page                        91
#define  Pragma_Passive                     92
#define  Pragma_Preelaborate                93
#define  Pragma_Priority                    94
#define  Pragma_Psect_Object                95
#define  Pragma_Pure                        96
#define  Pragma_Pure_Function               97
#define  Pragma_Remote_Call_Interface       98
#define  Pragma_Remote_Types                99
#define  Pragma_Share_Generic              100
#define  Pragma_Shared                     101
#define  Pragma_Shared_Passive             102
#define  Pragma_Source_Reference           103
#define  Pragma_Stream_Convert             104
#define  Pragma_Subtitle                   105
#define  Pragma_Suppress_All               106
#define  Pragma_Suppress_Debug_Info        107
#define  Pragma_Suppress_Initialization    108
#define  Pragma_System_Name                109
#define  Pragma_Task_Info                  110
#define  Pragma_Task_Name                  111
#define  Pragma_Task_Storage               112
#define  Pragma_Time_Slice                 113
#define  Pragma_Title                      114
#define  Pragma_Unchecked_Union            115
#define  Pragma_Unimplemented_Unit         116
#define  Pragma_Universal_Data             117
#define  Pragma_Unreferenced               118
#define  Pragma_Unreserve_All_Interrupts   119
#define  Pragma_Volatile                   120
#define  Pragma_Volatile_Components        121
#define  Pragma_Weak_External              122

/* The following are deliberately out of alphabetical order, see Snames */

#define  Pragma_AST_Entry                  123
#define  Pragma_Storage_Size               124
#define  Pragma_Storage_Unit               125

/* Define the numeric values for the conventions.  */

#define  Convention_Ada                      0
#define  Convention_Intrinsic                1
#define  Convention_Entry                    2
#define  Convention_Protected                3
#define  Convention_Assembler                4
#define  Convention_C                        5
#define  Convention_COBOL                    6
#define  Convention_CPP                      7
#define  Convention_Fortran                  8
#define  Convention_Java                     9
#define  Convention_Stdcall                 10
#define  Convention_Stubbed                 11
