/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S N A M E S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2003 Free Software Foundation, Inc.          *
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
#define Name_Space      (First_Name_Id + 256 + 3)
#define Name_Time       (First_Name_Id + 256 + 4)

/* Define the function to return one of the numeric values below. Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Attribute_Id snames__get_attribute_id
extern unsigned char Get_Attribute_Id (int);

/* Define the numeric values for attributes.  */

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
#define  Attr_Pool_Address                  64
#define  Attr_Pos                           65
#define  Attr_Position                      66
#define  Attr_Range                         67
#define  Attr_Range_Length                  68
#define  Attr_Round                         69
#define  Attr_Safe_Emax                     70
#define  Attr_Safe_First                    71
#define  Attr_Safe_Large                    72
#define  Attr_Safe_Last                     73
#define  Attr_Safe_Small                    74
#define  Attr_Scale                         75
#define  Attr_Scaling                       76
#define  Attr_Signed_Zeros                  77
#define  Attr_Size                          78
#define  Attr_Small                         79
#define  Attr_Storage_Size                  80
#define  Attr_Storage_Unit                  81
#define  Attr_Tag                           82
#define  Attr_Target_Name                   83
#define  Attr_Terminated                    84
#define  Attr_To_Address                    85
#define  Attr_Type_Class                    86
#define  Attr_UET_Address                   87
#define  Attr_Unbiased_Rounding             88
#define  Attr_Unchecked_Access              89
#define  Attr_Unconstrained_Array           90
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
extern Boolean Is_Pragma_Name (Name_Id);

/* Define the function to return one of the numeric values below.  Note
   that it actually returns a char since an enumeration value of less
   than 256 entries is represented that way in Ada.  The operand is a Chars
   field value.  */

#define Get_Pragma_Id snames__get_pragma_id
extern unsigned char Get_Pragma_Id (int);

/* Define the numeric values for the pragmas. */

/* Configuration pragmas first */

#define  Pragma_Ada_83                        0
#define  Pragma_Ada_95                        1
#define  Pragma_C_Pass_By_Copy                2
#define  Pragma_Compile_Time_Warning          3
#define  Pragma_Component_Alignment           4
#define  Pragma_Convention_Identifier         5
#define  Pragma_Discard_Names                 6
#define  Pragma_Elaboration_Checking          7
#define  Pragma_Eliminate                     8
#define  Pragma_Explicit_Overriding           9
#define  Pragma_Extend_System                10
#define  Pragma_Extensions_Allowed           11
#define  Pragma_External_Name_Casing         12
#define  Pragma_Float_Representation         13
#define  Pragma_Initialize_Scalars           14
#define  Pragma_Interrupt_State              15
#define  Pragma_License                      16
#define  Pragma_Locking_Policy               17
#define  Pragma_Long_Float                   18
#define  Pragma_No_Run_Time                  19
#define  Pragma_Normalize_Scalars            20
#define  Pragma_Polling                      21
#define  Pragma_Persistent_Data              22
#define  Pragma_Persistent_Object            23
#define  Pragma_Propagate_Exceptions         24
#define  Pragma_Queuing_Policy               25
#define  Pragma_Ravenscar                    26
#define  Pragma_Restricted_Run_Time          27
#define  Pragma_Restrictions                 28
#define  Pragma_Restriction_Warnings         29
#define  Pragma_Reviewable                   30
#define  Pragma_Source_File_Name             31
#define  Pragma_Source_File_Name_Project     32
#define  Pragma_Style_Checks                 33
#define  Pragma_Suppress                     34
#define  Pragma_Suppress_Exception_Locations 35
#define  Pragma_Task_Dispatching_Policy      36
#define  Pragma_Universal_Data               37
#define  Pragma_Unsuppress                   38
#define  Pragma_Use_VADS_Size                39
#define  Pragma_Validity_Checks              40
#define  Pragma_Warnings                     41

/* Remaining pragmas */

#define  Pragma_Abort_Defer                  42
#define  Pragma_All_Calls_Remote             43
#define  Pragma_Annotate                     44
#define  Pragma_Assert                       45
#define  Pragma_Asynchronous                 46
#define  Pragma_Atomic                       47
#define  Pragma_Atomic_Components            48
#define  Pragma_Attach_Handler               49
#define  Pragma_Comment                      50
#define  Pragma_Common_Object                51
#define  Pragma_Complex_Representation       52
#define  Pragma_Controlled                   53
#define  Pragma_Convention                   54
#define  Pragma_CPP_Class                    55
#define  Pragma_CPP_Constructor              56
#define  Pragma_CPP_Virtual                  57
#define  Pragma_CPP_Vtable                   58
#define  Pragma_Debug                        59
#define  Pragma_Elaborate                    60
#define  Pragma_Elaborate_All                61
#define  Pragma_Elaborate_Body               62
#define  Pragma_Export                       63
#define  Pragma_Export_Exception             64
#define  Pragma_Export_Function              65
#define  Pragma_Export_Object                66
#define  Pragma_Export_Procedure             67
#define  Pragma_Export_Value                 68
#define  Pragma_Export_Valued_Procedure      69
#define  Pragma_External                     70
#define  Pragma_Finalize_Storage_Only        71
#define  Pragma_Ident                        72
#define  Pragma_Import                       73
#define  Pragma_Import_Exception             74
#define  Pragma_Import_Function              75
#define  Pragma_Import_Object                76
#define  Pragma_Import_Procedure             77
#define  Pragma_Import_Valued_Procedure      78
#define  Pragma_Inline                       79
#define  Pragma_Inline_Always                80
#define  Pragma_Inline_Generic               81
#define  Pragma_Inspection_Point             82
#define  Pragma_Interface                    83
#define  Pragma_Interface_Name               84
#define  Pragma_Interrupt_Handler            85
#define  Pragma_Interrupt_Priority           86
#define  Pragma_Java_Constructor             87
#define  Pragma_Java_Interface               88
#define  Pragma_Keep_Names                   89
#define  Pragma_Link_With                    90
#define  Pragma_Linker_Alias                 91
#define  Pragma_Linker_Options               92
#define  Pragma_Linker_Section               93
#define  Pragma_List                         94
#define  Pragma_Machine_Attribute            95
#define  Pragma_Main                         96
#define  Pragma_Main_Storage                 97
#define  Pragma_Memory_Size                  98
#define  Pragma_No_Return                    99
#define  Pragma_Obsolescent                 100
#define  Pragma_Optimize                    101
#define  Pragma_Optional_Overriding         102
#define  Pragma_Overriding                  103
#define  Pragma_Pack                        104
#define  Pragma_Page                        105
#define  Pragma_Passive                     106
#define  Pragma_Preelaborate                107
#define  Pragma_Priority                    108
#define  Pragma_Psect_Object                109
#define  Pragma_Pure                        110
#define  Pragma_Pure_Function               111
#define  Pragma_Remote_Call_Interface       112
#define  Pragma_Remote_Types                113
#define  Pragma_Share_Generic               114
#define  Pragma_Shared                      115
#define  Pragma_Shared_Passive              116
#define  Pragma_Source_Reference            117
#define  Pragma_Stream_Convert              118
#define  Pragma_Subtitle                    119
#define  Pragma_Suppress_All                120
#define  Pragma_Suppress_Debug_Info         121
#define  Pragma_Suppress_Initialization     122
#define  Pragma_System_Name                 123
#define  Pragma_Task_Info                   124
#define  Pragma_Task_Name                   125
#define  Pragma_Task_Storage                126
#define  Pragma_Thread_Body                 127
#define  Pragma_Time_Slice                  128
#define  Pragma_Title                       129
#define  Pragma_Unchecked_Union             130
#define  Pragma_Unimplemented_Unit          131
#define  Pragma_Unreferenced                132
#define  Pragma_Unreserve_All_Interrupts    133
#define  Pragma_Volatile                    134
#define  Pragma_Volatile_Components         135
#define  Pragma_Weak_External               136

/* The following are deliberately out of alphabetical order, see Snames */

#define  Pragma_AST_Entry                   137
#define  Pragma_Storage_Size                138
#define  Pragma_Storage_Unit                139

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

/* End of snames.h (C version of Snames package spec) */
