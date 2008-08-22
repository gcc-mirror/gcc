/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                               S N A M E S                                *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2008, Free Software Foundation, Inc.         *
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
#define  Attr_Enum_Val                      29
#define  Attr_Epsilon                       30
#define  Attr_Exponent                      31
#define  Attr_External_Tag                  32
#define  Attr_Fast_Math                     33
#define  Attr_First                         34
#define  Attr_First_Bit                     35
#define  Attr_Fixed_Value                   36
#define  Attr_Fore                          37
#define  Attr_Has_Access_Values             38
#define  Attr_Has_Discriminants             39
#define  Attr_Has_Tagged_Values             40
#define  Attr_Identity                      41
#define  Attr_Img                           42
#define  Attr_Integer_Value                 43
#define  Attr_Invalid_Value                 44
#define  Attr_Large                         45
#define  Attr_Last                          46
#define  Attr_Last_Bit                      47
#define  Attr_Leading_Part                  48
#define  Attr_Length                        49
#define  Attr_Machine_Emax                  50
#define  Attr_Machine_Emin                  51
#define  Attr_Machine_Mantissa              52
#define  Attr_Machine_Overflows             53
#define  Attr_Machine_Radix                 54
#define  Attr_Machine_Rounding              55
#define  Attr_Machine_Rounds                56
#define  Attr_Machine_Size                  57
#define  Attr_Mantissa                      58
#define  Attr_Max_Size_In_Storage_Elements  59
#define  Attr_Maximum_Alignment             60
#define  Attr_Mechanism_Code                61
#define  Attr_Mod                           62
#define  Attr_Model_Emin                    63
#define  Attr_Model_Epsilon                 64
#define  Attr_Model_Mantissa                65
#define  Attr_Model_Small                   66
#define  Attr_Modulus                       67
#define  Attr_Null_Parameter                68
#define  Attr_Object_Size                   69
#define  Attr_Old                           70
#define  Attr_Partition_ID                  71
#define  Attr_Passed_By_Reference           72
#define  Attr_Pool_Address                  73
#define  Attr_Pos                           74
#define  Attr_Position                      75
#define  Attr_Priority                      76
#define  Attr_Range                         77
#define  Attr_Range_Length                  78
#define  Attr_Result                        79
#define  Attr_Round                         80
#define  Attr_Safe_Emax                     81
#define  Attr_Safe_First                    82
#define  Attr_Safe_Large                    83
#define  Attr_Safe_Last                     84
#define  Attr_Safe_Small                    85
#define  Attr_Scale                         86
#define  Attr_Scaling                       87
#define  Attr_Signed_Zeros                  88
#define  Attr_Size                          89
#define  Attr_Small                         90
#define  Attr_Storage_Size                  91
#define  Attr_Storage_Unit                  92
#define  Attr_Stream_Size                   93
#define  Attr_Tag                           94
#define  Attr_Target_Name                   95
#define  Attr_Terminated                    96
#define  Attr_To_Address                    97
#define  Attr_Type_Class                    98
#define  Attr_UET_Address                   99
#define  Attr_Unbiased_Rounding             100
#define  Attr_Unchecked_Access              101
#define  Attr_Unconstrained_Array           102
#define  Attr_Universal_Literal_String      103
#define  Attr_Unrestricted_Access           104
#define  Attr_VADS_Size                     105
#define  Attr_Val                           106
#define  Attr_Valid                         107
#define  Attr_Value_Size                    108
#define  Attr_Version                       109
#define  Attr_Wchar_T_Size                  110
#define  Attr_Wide_Wide_Width               111
#define  Attr_Wide_Width                    112
#define  Attr_Width                         113
#define  Attr_Word_Size                     114
#define  Attr_Adjacent                      115
#define  Attr_Ceiling                       116
#define  Attr_Copy_Sign                     117
#define  Attr_Floor                         118
#define  Attr_Fraction                      119
#define  Attr_From_Any                      120
#define  Attr_Image                         121
#define  Attr_Input                         122
#define  Attr_Machine                       123
#define  Attr_Max                           124
#define  Attr_Min                           125
#define  Attr_Model                         126
#define  Attr_Pred                          127
#define  Attr_Remainder                     128
#define  Attr_Rounding                      129
#define  Attr_Succ                          130
#define  Attr_To_Any                        131
#define  Attr_Truncation                    132
#define  Attr_TypeCode                      133
#define  Attr_Value                         134
#define  Attr_Wide_Image                    135
#define  Attr_Wide_Wide_Image               136
#define  Attr_Wide_Value                    137
#define  Attr_Wide_Wide_Value               138
#define  Attr_Output                        139
#define  Attr_Read                          140
#define  Attr_Write                         141
#define  Attr_Elab_Body                     142
#define  Attr_Elab_Spec                     143
#define  Attr_Storage_Pool                  144
#define  Attr_Base                          145
#define  Attr_Class                         146
#define  Attr_Stub_Type                     147

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
#define  Pragma_Assume_No_Invalid_Values      5
#define  Pragma_C_Pass_By_Copy                6
#define  Pragma_Check_Name                    7
#define  Pragma_Check_Policy                  8
#define  Pragma_Compile_Time_Error            9
#define  Pragma_Compile_Time_Warning          10
#define  Pragma_Compiler_Unit                 11
#define  Pragma_Component_Alignment           12
#define  Pragma_Convention_Identifier         13
#define  Pragma_Debug_Policy                  14
#define  Pragma_Detect_Blocking               15
#define  Pragma_Discard_Names                 16
#define  Pragma_Elaboration_Checks            17
#define  Pragma_Eliminate                     18
#define  Pragma_Extend_System                 19
#define  Pragma_Extensions_Allowed            20
#define  Pragma_External_Name_Casing          21
#define  Pragma_Favor_Top_Level               22
#define  Pragma_Float_Representation          23
#define  Pragma_Implicit_Packing              24
#define  Pragma_Initialize_Scalars            25
#define  Pragma_Interrupt_State               26
#define  Pragma_License                       27
#define  Pragma_Locking_Policy                28
#define  Pragma_Long_Float                    29
#define  Pragma_No_Run_Time                   30
#define  Pragma_No_Strict_Aliasing            31
#define  Pragma_Normalize_Scalars             32
#define  Pragma_Optimize_Alignment            33
#define  Pragma_Persistent_BSS                34
#define  Pragma_Polling                       35
#define  Pragma_Priority_Specific_Dispatching 36
#define  Pragma_Profile                       37
#define  Pragma_Profile_Warnings              38
#define  Pragma_Propagate_Exceptions          39
#define  Pragma_Queuing_Policy                40
#define  Pragma_Ravenscar                     41
#define  Pragma_Restricted_Run_Time           42
#define  Pragma_Restrictions                  43
#define  Pragma_Restriction_Warnings          44
#define  Pragma_Reviewable                    45
#define  Pragma_Source_File_Name              46
#define  Pragma_Source_File_Name_Project      47
#define  Pragma_Style_Checks                  48
#define  Pragma_Suppress                      49
#define  Pragma_Suppress_Exception_Locations  50
#define  Pragma_Task_Dispatching_Policy       51
#define  Pragma_Universal_Data                52
#define  Pragma_Unsuppress                    53
#define  Pragma_Use_VADS_Size                 54
#define  Pragma_Validity_Checks               55
#define  Pragma_Warnings                      56
#define  Pragma_Wide_Character_Encoding       57
#define  Pragma_Abort_Defer                   58
#define  Pragma_All_Calls_Remote              59
#define  Pragma_Annotate                      60
#define  Pragma_Assert                        61
#define  Pragma_Asynchronous                  62
#define  Pragma_Atomic                        63
#define  Pragma_Atomic_Components             64
#define  Pragma_Attach_Handler                65
#define  Pragma_Check                         66
#define  Pragma_CIL_Constructor               67
#define  Pragma_Comment                       68
#define  Pragma_Common_Object                 69
#define  Pragma_Complete_Representation       70
#define  Pragma_Complex_Representation        71
#define  Pragma_Controlled                    72
#define  Pragma_Convention                    73
#define  Pragma_CPP_Class                     74
#define  Pragma_CPP_Constructor               75
#define  Pragma_CPP_Virtual                   76
#define  Pragma_CPP_Vtable                    77
#define  Pragma_Debug                         78
#define  Pragma_Elaborate                     79
#define  Pragma_Elaborate_All                 80
#define  Pragma_Elaborate_Body                81
#define  Pragma_Export                        82
#define  Pragma_Export_Exception              83
#define  Pragma_Export_Function               84
#define  Pragma_Export_Object                 85
#define  Pragma_Export_Procedure              86
#define  Pragma_Export_Value                  87
#define  Pragma_Export_Valued_Procedure       88
#define  Pragma_External                      89
#define  Pragma_Finalize_Storage_Only         90
#define  Pragma_Ident                         91
#define  Pragma_Implemented_By_Entry          92
#define  Pragma_Import                        93
#define  Pragma_Import_Exception              94
#define  Pragma_Import_Function               95
#define  Pragma_Import_Object                 96
#define  Pragma_Import_Procedure              97
#define  Pragma_Import_Valued_Procedure       98
#define  Pragma_Inline                        99
#define  Pragma_Inline_Always                 100
#define  Pragma_Inline_Generic                101
#define  Pragma_Inspection_Point              102
#define  Pragma_Interface_Name                103
#define  Pragma_Interrupt_Handler             104
#define  Pragma_Interrupt_Priority            105
#define  Pragma_Java_Constructor              106
#define  Pragma_Java_Interface                107
#define  Pragma_Keep_Names                    108
#define  Pragma_Link_With                     109
#define  Pragma_Linker_Alias                  110
#define  Pragma_Linker_Constructor            111
#define  Pragma_Linker_Destructor             112
#define  Pragma_Linker_Options                113
#define  Pragma_Linker_Section                114
#define  Pragma_List                          115
#define  Pragma_Machine_Attribute             116
#define  Pragma_Main                          117
#define  Pragma_Main_Storage                  118
#define  Pragma_Memory_Size                   119
#define  Pragma_No_Body                       120
#define  Pragma_No_Return                     121
#define  Pragma_Obsolescent                   122
#define  Pragma_Optimize                      123
#define  Pragma_Pack                          124
#define  Pragma_Page                          125
#define  Pragma_Passive                       126
#define  Pragma_Postcondition                 127
#define  Pragma_Precondition                  128
#define  Pragma_Preelaborable_Initialization  129
#define  Pragma_Preelaborate                  130
#define  Pragma_Preelaborate_05               131
#define  Pragma_Psect_Object                  132
#define  Pragma_Pure                          133
#define  Pragma_Pure_05                       134
#define  Pragma_Pure_Function                 135
#define  Pragma_Relative_Deadline             136
#define  Pragma_Remote_Call_Interface         137
#define  Pragma_Remote_Types                  138
#define  Pragma_Share_Generic                 139
#define  Pragma_Shared                        140
#define  Pragma_Shared_Passive                141
#define  Pragma_Source_Reference              142
#define  Pragma_Static_Elaboration_Desired    143
#define  Pragma_Stream_Convert                144
#define  Pragma_Subtitle                      145
#define  Pragma_Suppress_All                  146
#define  Pragma_Suppress_Debug_Info           147
#define  Pragma_Suppress_Initialization       148
#define  Pragma_System_Name                   149
#define  Pragma_Task_Info                     150
#define  Pragma_Task_Name                     151
#define  Pragma_Task_Storage                  152
#define  Pragma_Time_Slice                    153
#define  Pragma_Title                         154
#define  Pragma_Unchecked_Union               155
#define  Pragma_Unimplemented_Unit            156
#define  Pragma_Universal_Aliasing            157
#define  Pragma_Unmodified                    158
#define  Pragma_Unreferenced                  159
#define  Pragma_Unreferenced_Objects          160
#define  Pragma_Unreserve_All_Interrupts      161
#define  Pragma_Volatile                      162
#define  Pragma_Volatile_Components           163
#define  Pragma_Weak_External                 164
#define  Pragma_AST_Entry                     165
#define  Pragma_Fast_Math                     166
#define  Pragma_Interface                     167
#define  Pragma_Priority                      168
#define  Pragma_Storage_Size                  169
#define  Pragma_Storage_Unit                  170

/* End of snames.h (C version of Snames package spec) */
