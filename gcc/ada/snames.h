/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S N A M E S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2007, Free Software Foundation, Inc.         *
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
   Snames. It was created automatically from the file snames.ads. */

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

#define  Attr_Abort_Signal                  0
#define  Attr_Access                        1
#define  Attr_Address                       2
#define  Attr_Address_Size                  3
#define  Attr_Aft                           4
#define  Attr_Alignment                     5
#define  Attr_Asm_Input                     6
#define  Attr_Asm_Output                    7
#define  Attr_AST_Entry                     8
#define  Attr_Bit                           9
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
#define  Attr_Enabled                       27
#define  Attr_Enum_Rep                      28
#define  Attr_Epsilon                       29
#define  Attr_Exponent                      30
#define  Attr_External_Tag                  31
#define  Attr_Fast_Math                     32
#define  Attr_First                         33
#define  Attr_First_Bit                     34
#define  Attr_Fixed_Value                   35
#define  Attr_Fore                          36
#define  Attr_Has_Access_Values             37
#define  Attr_Has_Discriminants             38
#define  Attr_Identity                      39
#define  Attr_Img                           40
#define  Attr_Integer_Value                 41
#define  Attr_Large                         42
#define  Attr_Last                          43
#define  Attr_Last_Bit                      44
#define  Attr_Leading_Part                  45
#define  Attr_Length                        46
#define  Attr_Machine_Emax                  47
#define  Attr_Machine_Emin                  48
#define  Attr_Machine_Mantissa              49
#define  Attr_Machine_Overflows             50
#define  Attr_Machine_Radix                 51
#define  Attr_Machine_Rounding              52
#define  Attr_Machine_Rounds                53
#define  Attr_Machine_Size                  54
#define  Attr_Mantissa                      55
#define  Attr_Max_Size_In_Storage_Elements  56
#define  Attr_Maximum_Alignment             57
#define  Attr_Mechanism_Code                58
#define  Attr_Mod                           59
#define  Attr_Model_Emin                    60
#define  Attr_Model_Epsilon                 61
#define  Attr_Model_Mantissa                62
#define  Attr_Model_Small                   63
#define  Attr_Modulus                       64
#define  Attr_Null_Parameter                65
#define  Attr_Object_Size                   66
#define  Attr_Old                           67
#define  Attr_Partition_ID                  68
#define  Attr_Passed_By_Reference           69
#define  Attr_Pool_Address                  70
#define  Attr_Pos                           71
#define  Attr_Position                      72
#define  Attr_Priority                      73
#define  Attr_Range                         74
#define  Attr_Range_Length                  75
#define  Attr_Round                         76
#define  Attr_Safe_Emax                     77
#define  Attr_Safe_First                    78
#define  Attr_Safe_Large                    79
#define  Attr_Safe_Last                     80
#define  Attr_Safe_Small                    81
#define  Attr_Scale                         82
#define  Attr_Scaling                       83
#define  Attr_Signed_Zeros                  84
#define  Attr_Size                          85
#define  Attr_Small                         86
#define  Attr_Storage_Size                  87
#define  Attr_Storage_Unit                  88
#define  Attr_Stream_Size                   89
#define  Attr_Tag                           90
#define  Attr_Target_Name                   91
#define  Attr_Terminated                    92
#define  Attr_To_Address                    93
#define  Attr_Type_Class                    94
#define  Attr_UET_Address                   95
#define  Attr_Unbiased_Rounding             96
#define  Attr_Unchecked_Access              97
#define  Attr_Unconstrained_Array           98
#define  Attr_Universal_Literal_String      99
#define  Attr_Unrestricted_Access           100
#define  Attr_VADS_Size                     101
#define  Attr_Val                           102
#define  Attr_Valid                         103
#define  Attr_Value_Size                    104
#define  Attr_Version                       105
#define  Attr_Wchar_T_Size                  106
#define  Attr_Wide_Wide_Width               107
#define  Attr_Wide_Width                    108
#define  Attr_Width                         109
#define  Attr_Word_Size                     110
#define  Attr_Adjacent                      111
#define  Attr_Ceiling                       112
#define  Attr_Copy_Sign                     113
#define  Attr_Floor                         114
#define  Attr_Fraction                      115
#define  Attr_Image                         116
#define  Attr_Input                         117
#define  Attr_Machine                       118
#define  Attr_Max                           119
#define  Attr_Min                           120
#define  Attr_Model                         121
#define  Attr_Pred                          122
#define  Attr_Remainder                     123
#define  Attr_Rounding                      124
#define  Attr_Succ                          125
#define  Attr_Truncation                    126
#define  Attr_Value                         127
#define  Attr_Wide_Image                    128
#define  Attr_Wide_Wide_Image               129
#define  Attr_Wide_Value                    130
#define  Attr_Wide_Wide_Value               131
#define  Attr_Output                        132
#define  Attr_Read                          133
#define  Attr_Write                         134
#define  Attr_Elab_Body                     135
#define  Attr_Elab_Spec                     136
#define  Attr_Storage_Pool                  137
#define  Attr_Base                          138
#define  Attr_Class                         139
#define  Attr_Stub_Type                     140

/* Define the numeric values for the conventions.  */

#define  Convention_Ada                           0
#define  Convention_Intrinsic                     1
#define  Convention_Entry                         2
#define  Convention_Protected                     3
#define  Convention_Stubbed                       4
#define  Convention_Assembler                     5
#define  Convention_C                             6
#define  Convention_CIL                           7
#define  Convention_COBOL                         8
#define  Convention_CPP                           9
#define  Convention_Fortran                       10
#define  Convention_Java                          11
#define  Convention_Stdcall                       12

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

#define  Pragma_Ada_83                        0
#define  Pragma_Ada_95                        1
#define  Pragma_Ada_05                        2
#define  Pragma_Ada_2005                      3
#define  Pragma_Assertion_Policy              4
#define  Pragma_C_Pass_By_Copy                5
#define  Pragma_Check_Name                    6
#define  Pragma_Compile_Time_Error            7
#define  Pragma_Compile_Time_Warning          8
#define  Pragma_Compiler_Unit                 9
#define  Pragma_Component_Alignment           10
#define  Pragma_Convention_Identifier         11
#define  Pragma_Debug_Policy                  12
#define  Pragma_Detect_Blocking               13
#define  Pragma_Discard_Names                 14
#define  Pragma_Elaboration_Checks            15
#define  Pragma_Eliminate                     16
#define  Pragma_Extend_System                 17
#define  Pragma_Extensions_Allowed            18
#define  Pragma_External_Name_Casing          19
#define  Pragma_Favor_Top_Level               20
#define  Pragma_Float_Representation          21
#define  Pragma_Implicit_Packing              22
#define  Pragma_Initialize_Scalars            23
#define  Pragma_Interrupt_State               24
#define  Pragma_License                       25
#define  Pragma_Locking_Policy                26
#define  Pragma_Long_Float                    27
#define  Pragma_No_Run_Time                   28
#define  Pragma_No_Strict_Aliasing            29
#define  Pragma_Normalize_Scalars             30
#define  Pragma_Optimize_Alignment            31
#define  Pragma_Polling                       32
#define  Pragma_Persistent_BSS                33
#define  Pragma_Priority_Specific_Dispatching 34
#define  Pragma_Profile                       35
#define  Pragma_Profile_Warnings              36
#define  Pragma_Propagate_Exceptions          37
#define  Pragma_Queuing_Policy                38
#define  Pragma_Ravenscar                     39
#define  Pragma_Restricted_Run_Time           40
#define  Pragma_Restrictions                  41
#define  Pragma_Restriction_Warnings          42
#define  Pragma_Reviewable                    43
#define  Pragma_Source_File_Name              44
#define  Pragma_Source_File_Name_Project      45
#define  Pragma_Style_Checks                  46
#define  Pragma_Suppress                      47
#define  Pragma_Suppress_Exception_Locations  48
#define  Pragma_Task_Dispatching_Policy       49
#define  Pragma_Universal_Data                50
#define  Pragma_Unsuppress                    51
#define  Pragma_Use_VADS_Size                 52
#define  Pragma_Validity_Checks               53
#define  Pragma_Warnings                      54
#define  Pragma_Wide_Character_Encoding       55
#define  Pragma_Abort_Defer                   56
#define  Pragma_All_Calls_Remote              57
#define  Pragma_Annotate                      58
#define  Pragma_Assert                        59
#define  Pragma_Asynchronous                  60
#define  Pragma_Atomic                        61
#define  Pragma_Atomic_Components             62
#define  Pragma_Attach_Handler                63
#define  Pragma_CIL_Constructor               64
#define  Pragma_Comment                       65
#define  Pragma_Common_Object                 66
#define  Pragma_Complete_Representation       67
#define  Pragma_Complex_Representation        68
#define  Pragma_Controlled                    69
#define  Pragma_Convention                    70
#define  Pragma_CPP_Class                     71
#define  Pragma_CPP_Constructor               72
#define  Pragma_CPP_Virtual                   73
#define  Pragma_CPP_Vtable                    74
#define  Pragma_Debug                         75
#define  Pragma_Elaborate                     76
#define  Pragma_Elaborate_All                 77
#define  Pragma_Elaborate_Body                78
#define  Pragma_Export                        79
#define  Pragma_Export_Exception              80
#define  Pragma_Export_Function               81
#define  Pragma_Export_Object                 82
#define  Pragma_Export_Procedure              83
#define  Pragma_Export_Value                  84
#define  Pragma_Export_Valued_Procedure       85
#define  Pragma_External                      86
#define  Pragma_Finalize_Storage_Only         87
#define  Pragma_Ident                         88
#define  Pragma_Implemented_By_Entry          89
#define  Pragma_Import                        90
#define  Pragma_Import_Exception              91
#define  Pragma_Import_Function               92
#define  Pragma_Import_Object                 93
#define  Pragma_Import_Procedure              94
#define  Pragma_Import_Valued_Procedure       95
#define  Pragma_Inline                        96
#define  Pragma_Inline_Always                 97
#define  Pragma_Inline_Generic                98
#define  Pragma_Inspection_Point              99
#define  Pragma_Interface_Name                100
#define  Pragma_Interrupt_Handler             101
#define  Pragma_Interrupt_Priority            102
#define  Pragma_Java_Constructor              103
#define  Pragma_Java_Interface                104
#define  Pragma_Keep_Names                    105
#define  Pragma_Link_With                     106
#define  Pragma_Linker_Alias                  107
#define  Pragma_Linker_Constructor            108
#define  Pragma_Linker_Destructor             109
#define  Pragma_Linker_Options                110
#define  Pragma_Linker_Section                111
#define  Pragma_List                          112
#define  Pragma_Machine_Attribute             113
#define  Pragma_Main                          114
#define  Pragma_Main_Storage                  115
#define  Pragma_Memory_Size                   116
#define  Pragma_No_Body                       117
#define  Pragma_No_Return                     118
#define  Pragma_Obsolescent                   119
#define  Pragma_Optimize                      120
#define  Pragma_Pack                          121
#define  Pragma_Page                          122
#define  Pragma_Passive                       123
#define  Pragma_Preelaborable_Initialization  124
#define  Pragma_Preelaborate                  125
#define  Pragma_Preelaborate_05               126
#define  Pragma_Psect_Object                  127
#define  Pragma_Pure                          128
#define  Pragma_Pure_05                       129
#define  Pragma_Pure_Function                 130
#define  Pragma_Remote_Call_Interface         131
#define  Pragma_Remote_Types                  132
#define  Pragma_Share_Generic                 133
#define  Pragma_Shared                        134
#define  Pragma_Shared_Passive                135
#define  Pragma_Source_Reference              136
#define  Pragma_Static_Elaboration_Desired    137
#define  Pragma_Stream_Convert                138
#define  Pragma_Subtitle                      139
#define  Pragma_Suppress_All                  140
#define  Pragma_Suppress_Debug_Info           141
#define  Pragma_Suppress_Initialization       142
#define  Pragma_System_Name                   143
#define  Pragma_Task_Info                     144
#define  Pragma_Task_Name                     145
#define  Pragma_Task_Storage                  146
#define  Pragma_Time_Slice                    147
#define  Pragma_Title                         148
#define  Pragma_Unchecked_Union               149
#define  Pragma_Unimplemented_Unit            150
#define  Pragma_Universal_Aliasing            151
#define  Pragma_Unmodified                    152
#define  Pragma_Unreferenced                  153
#define  Pragma_Unreferenced_Objects          154
#define  Pragma_Unreserve_All_Interrupts      155
#define  Pragma_Volatile                      156
#define  Pragma_Volatile_Components           157
#define  Pragma_Weak_External                 158
#define  Pragma_AST_Entry                     159
#define  Pragma_Fast_Math                     160
#define  Pragma_Interface                     161
#define  Pragma_Priority                      162
#define  Pragma_Storage_Size                  163
#define  Pragma_Storage_Unit                  164

/* End of snames.h (C version of Snames package spec) */
