/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S N A M E S                                *
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
#define  Attr_Max_Interrupt_Priority        52
#define  Attr_Max_Priority                  53
#define  Attr_Max_Size_In_Storage_Elements  54
#define  Attr_Maximum_Alignment             55
#define  Attr_Mechanism_Code                56
#define  Attr_Model_Emin                    57
#define  Attr_Model_Epsilon                 58
#define  Attr_Model_Mantissa                59
#define  Attr_Model_Small                   60
#define  Attr_Modulus                       61
#define  Attr_Null_Parameter                62
#define  Attr_Object_Size                   63
#define  Attr_Partition_ID                  64
#define  Attr_Passed_By_Reference           65
#define  Attr_Pos                           66
#define  Attr_Position                      67
#define  Attr_Range                         68
#define  Attr_Range_Length                  69
#define  Attr_Round                         70
#define  Attr_Safe_Emax                     71
#define  Attr_Safe_First                    72
#define  Attr_Safe_Large                    73
#define  Attr_Safe_Last                     74
#define  Attr_Safe_Small                    75
#define  Attr_Scale                         76
#define  Attr_Scaling                       77
#define  Attr_Signed_Zeros                  78
#define  Attr_Size                          79
#define  Attr_Small                         80
#define  Attr_Storage_Size                  81
#define  Attr_Storage_Unit                  82
#define  Attr_Tag                           83
#define  Attr_Terminated                    84
#define  Attr_Tick                          85
#define  Attr_To_Address                    86
#define  Attr_Type_Class                    87
#define  Attr_UET_Address                   88
#define  Attr_Unbiased_Rounding             89
#define  Attr_Unchecked_Access              90
#define  Attr_Universal_Literal_String      91
#define  Attr_Unrestricted_Access           92
#define  Attr_VADS_Size                     93
#define  Attr_Val                           94
#define  Attr_Valid                         95
#define  Attr_Value_Size                    96
#define  Attr_Version                       97
#define  Attr_Wide_Character_Size           98
#define  Attr_Wide_Width                    99
#define  Attr_Width                        100
#define  Attr_Word_Size                    101

#define  Attr_Adjacent                     102
#define  Attr_Ceiling                      103
#define  Attr_Copy_Sign                    104
#define  Attr_Floor                        105
#define  Attr_Fraction                     106
#define  Attr_Image                        107
#define  Attr_Input                        108
#define  Attr_Machine                      109
#define  Attr_Max                          110
#define  Attr_Min                          111
#define  Attr_Model                        112
#define  Attr_Pred                         113
#define  Attr_Remainder                    114
#define  Attr_Rounding                     115
#define  Attr_Succ                         116
#define  Attr_Truncation                   117
#define  Attr_Value                        118
#define  Attr_Wide_Image                   119
#define  Attr_Wide_Value                   120

#define  Attr_Output                       121
#define  Attr_Read                         122
#define  Attr_Write                        123

#define  Attr_Elab_Body                    124
#define  Attr_Elab_Spec                    125
#define  Attr_Storage_Pool                 126

#define  Attr_Base                         127
#define  Attr_Class                        128

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
#define  Pragma_Discard_Names                4
#define  Pragma_Elaboration_Checking         5
#define  Pragma_Eliminate                    6
#define  Pragma_Extend_System                7
#define  Pragma_Extensions_Allowed           8
#define  Pragma_External_Name_Casing         9
#define  Pragma_Float_Representation        10
#define  Pragma_Initialize                  11
#define  Pragma_License                     12
#define  Pragma_Locking_Policy              13
#define  Pragma_Long_Float                  14
#define  Pragma_No_Run_Time                 15
#define  Pragma_Normalize_Scalars           16
#define  Pragma_Polling                     17
#define  Pragma_Propagate_Exceptions        18
#define  Pragma_Queuing_Policy              19
#define  Pragma_Ravenscar                   20
#define  Pragma_Restricted_Run_Time         21
#define  Pragma_Restrictions                22
#define  Pragma_Reviewable                  23
#define  Pragma_Source_File_Name            24
#define  Pragma_Style_Checks                25
#define  Pragma_Suppress                    26
#define  Pragma_Task_Dispatching_Policy     27
#define  Pragma_Unsuppress                  28
#define  Pragma_Use_VADS_Size               29
#define  Pragma_Validity_Checks             30
#define  Pragma_Warnings                    31

/* Remaining pragmas */

#define  Pragma_Abort_Defer                 32
#define  Pragma_All_Calls_Remote            33
#define  Pragma_Annotate                    34
#define  Pragma_Assert                      35
#define  Pragma_Asynchronous                36
#define  Pragma_Atomic                      37
#define  Pragma_Atomic_Components           38
#define  Pragma_Attach_Handler              39
#define  Pragma_Comment                     40
#define  Pragma_Common_Object               41
#define  Pragma_Complex_Representation      42
#define  Pragma_Controlled                  43
#define  Pragma_Convention                  44
#define  Pragma_CPP_Class                   45
#define  Pragma_CPP_Constructor             46
#define  Pragma_CPP_Virtual                 47
#define  Pragma_CPP_Vtable                  48
#define  Pragma_Debug                       49
#define  Pragma_Elaborate                   50
#define  Pragma_Elaborate_All               51
#define  Pragma_Elaborate_Body              52
#define  Pragma_Export                      53
#define  Pragma_Export_Exception            54
#define  Pragma_Export_Function             55
#define  Pragma_Export_Object               56
#define  Pragma_Export_Procedure            57
#define  Pragma_Export_Valued_Procedure     58
#define  Pragma_External                    59
#define  Pragma_Finalize_Storage_Only       60
#define  Pragma_Ident                       61
#define  Pragma_Import                      62
#define  Pragma_Import_Exception            63
#define  Pragma_Import_Function             64
#define  Pragma_Import_Object               65
#define  Pragma_Import_Procedure            66
#define  Pragma_Import_Valued_Procedure     67
#define  Pragma_Inline                      68
#define  Pragma_Inline_Always               69
#define  Pragma_Inline_Generic              70
#define  Pragma_Inspection_Point            71
#define  Pragma_Interface                   72
#define  Pragma_Interface_Name              73
#define  Pragma_Interrupt_Handler           74
#define  Pragma_Interrupt_Priority          75
#define  Pragma_Java_Constructor            76
#define  Pragma_Java_Interface              77
#define  Pragma_Link_With                   78
#define  Pragma_Linker_Alias                79
#define  Pragma_Linker_Options              80
#define  Pragma_Linker_Section              81
#define  Pragma_List                        82
#define  Pragma_Machine_Attribute           83
#define  Pragma_Main                        84
#define  Pragma_Main_Storage                85
#define  Pragma_Memory_Size                 86
#define  Pragma_No_Return                   87
#define  Pragma_Optimize                    88
#define  Pragma_Pack                        89
#define  Pragma_Page                        90
#define  Pragma_Passive                     91
#define  Pragma_Preelaborate                92
#define  Pragma_Priority                    93
#define  Pragma_Psect_Object                94
#define  Pragma_Pure                        95
#define  Pragma_Pure_Function               96
#define  Pragma_Remote_Call_Interface       97
#define  Pragma_Remote_Types                98
#define  Pragma_Share_Generic               99
#define  Pragma_Shared                     100
#define  Pragma_Shared_Passive             101
#define  Pragma_Source_Reference           102
#define  Pragma_Stream_Convert             103
#define  Pragma_Subtitle                   104
#define  Pragma_Suppress_All               105
#define  Pragma_Suppress_Debug_Info        106
#define  Pragma_Suppress_Initialization    107
#define  Pragma_System_Name                108
#define  Pragma_Task_Info                  109
#define  Pragma_Task_Name                  110
#define  Pragma_Task_Storage               111
#define  Pragma_Time_Slice                 112
#define  Pragma_Title                      113
#define  Pragma_Unchecked_Union            114
#define  Pragma_Unimplemented_Unit         115
#define  Pragma_Unreserve_All_Interrupts   116
#define  Pragma_Volatile                   117
#define  Pragma_Volatile_Components        118
#define  Pragma_Weak_External              119

/* The following are deliberately out of alphabetical order, see Snames */

#define  Pragma_AST_Entry                  120
#define  Pragma_Storage_Size               121
#define  Pragma_Storage_Unit               122

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
