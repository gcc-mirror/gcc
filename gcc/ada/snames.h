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
#define  Attr_First                         32
#define  Attr_First_Bit                     33
#define  Attr_Fixed_Value                   34
#define  Attr_Fore                          35
#define  Attr_Has_Access_Values             36
#define  Attr_Has_Discriminants             37
#define  Attr_Identity                      38
#define  Attr_Img                           39
#define  Attr_Integer_Value                 40
#define  Attr_Large                         41
#define  Attr_Last                          42
#define  Attr_Last_Bit                      43
#define  Attr_Leading_Part                  44
#define  Attr_Length                        45
#define  Attr_Machine_Emax                  46
#define  Attr_Machine_Emin                  47
#define  Attr_Machine_Mantissa              48
#define  Attr_Machine_Overflows             49
#define  Attr_Machine_Radix                 50
#define  Attr_Machine_Rounding              51
#define  Attr_Machine_Rounds                52
#define  Attr_Machine_Size                  53
#define  Attr_Mantissa                      54
#define  Attr_Max_Size_In_Storage_Elements  55
#define  Attr_Maximum_Alignment             56
#define  Attr_Mechanism_Code                57
#define  Attr_Mod                           58
#define  Attr_Model_Emin                    59
#define  Attr_Model_Epsilon                 60
#define  Attr_Model_Mantissa                61
#define  Attr_Model_Small                   62
#define  Attr_Modulus                       63
#define  Attr_Null_Parameter                64
#define  Attr_Object_Size                   65
#define  Attr_Partition_ID                  66
#define  Attr_Passed_By_Reference           67
#define  Attr_Pool_Address                  68
#define  Attr_Pos                           69
#define  Attr_Position                      70
#define  Attr_Priority                      71
#define  Attr_Range                         72
#define  Attr_Range_Length                  73
#define  Attr_Round                         74
#define  Attr_Safe_Emax                     75
#define  Attr_Safe_First                    76
#define  Attr_Safe_Large                    77
#define  Attr_Safe_Last                     78
#define  Attr_Safe_Small                    79
#define  Attr_Scale                         80
#define  Attr_Scaling                       81
#define  Attr_Signed_Zeros                  82
#define  Attr_Size                          83
#define  Attr_Small                         84
#define  Attr_Storage_Size                  85
#define  Attr_Storage_Unit                  86
#define  Attr_Stream_Size                   87
#define  Attr_Tag                           88
#define  Attr_Target_Name                   89
#define  Attr_Terminated                    90
#define  Attr_To_Address                    91
#define  Attr_Type_Class                    92
#define  Attr_UET_Address                   93
#define  Attr_Unbiased_Rounding             94
#define  Attr_Unchecked_Access              95
#define  Attr_Unconstrained_Array           96
#define  Attr_Universal_Literal_String      97
#define  Attr_Unrestricted_Access           98
#define  Attr_VADS_Size                     99
#define  Attr_Val                           100
#define  Attr_Valid                         101
#define  Attr_Value_Size                    102
#define  Attr_Version                       103
#define  Attr_Wchar_T_Size                  104
#define  Attr_Wide_Wide_Width               105
#define  Attr_Wide_Width                    106
#define  Attr_Width                         107
#define  Attr_Word_Size                     108
#define  Attr_Adjacent                      109
#define  Attr_Ceiling                       110
#define  Attr_Copy_Sign                     111
#define  Attr_Floor                         112
#define  Attr_Fraction                      113
#define  Attr_Image                         114
#define  Attr_Input                         115
#define  Attr_Machine                       116
#define  Attr_Max                           117
#define  Attr_Min                           118
#define  Attr_Model                         119
#define  Attr_Pred                          120
#define  Attr_Remainder                     121
#define  Attr_Rounding                      122
#define  Attr_Succ                          123
#define  Attr_Truncation                    124
#define  Attr_Value                         125
#define  Attr_Wide_Image                    126
#define  Attr_Wide_Wide_Image               127
#define  Attr_Wide_Value                    128
#define  Attr_Wide_Wide_Value               129
#define  Attr_Output                        130
#define  Attr_Read                          131
#define  Attr_Write                         132
#define  Attr_Elab_Body                     133
#define  Attr_Elab_Spec                     134
#define  Attr_Storage_Pool                  135
#define  Attr_Base                          136
#define  Attr_Class                         137
#define  Attr_Stub_Type                     138

/* Define the numeric values for the conventions.  */

#define  Convention_Ada                           0
#define  Convention_Intrinsic                     1
#define  Convention_Entry                         2
#define  Convention_Protected                     3
#define  Convention_Assembler                     4
#define  Convention_C                             5
#define  Convention_CIL                           6
#define  Convention_COBOL                         7
#define  Convention_CPP                           8
#define  Convention_Fortran                       9
#define  Convention_Java                          10
#define  Convention_Stdcall                       11
#define  Convention_Stubbed                       12

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
#define  Pragma_Component_Alignment           9
#define  Pragma_Convention_Identifier         10
#define  Pragma_Debug_Policy                  11
#define  Pragma_Detect_Blocking               12
#define  Pragma_Discard_Names                 13
#define  Pragma_Elaboration_Checks            14
#define  Pragma_Eliminate                     15
#define  Pragma_Extend_System                 16
#define  Pragma_Extensions_Allowed            17
#define  Pragma_External_Name_Casing          18
#define  Pragma_Float_Representation          19
#define  Pragma_Implicit_Packing              20
#define  Pragma_Initialize_Scalars            21
#define  Pragma_Interrupt_State               22
#define  Pragma_License                       23
#define  Pragma_Locking_Policy                24
#define  Pragma_Long_Float                    25
#define  Pragma_No_Run_Time                   26
#define  Pragma_No_Strict_Aliasing            27
#define  Pragma_Normalize_Scalars             28
#define  Pragma_Polling                       29
#define  Pragma_Persistent_BSS                30
#define  Pragma_Priority_Specific_Dispatching 31
#define  Pragma_Profile                       32
#define  Pragma_Profile_Warnings              33
#define  Pragma_Propagate_Exceptions          34
#define  Pragma_Queuing_Policy                35
#define  Pragma_Ravenscar                     36
#define  Pragma_Restricted_Run_Time           37
#define  Pragma_Restrictions                  38
#define  Pragma_Restriction_Warnings          39
#define  Pragma_Reviewable                    40
#define  Pragma_Source_File_Name              41
#define  Pragma_Source_File_Name_Project      42
#define  Pragma_Style_Checks                  43
#define  Pragma_Suppress                      44
#define  Pragma_Suppress_Exception_Locations  45
#define  Pragma_Task_Dispatching_Policy       46
#define  Pragma_Universal_Data                47
#define  Pragma_Unsuppress                    48
#define  Pragma_Use_VADS_Size                 49
#define  Pragma_Validity_Checks               50
#define  Pragma_Warnings                      51
#define  Pragma_Wide_Character_Encoding       52
#define  Pragma_Abort_Defer                   53
#define  Pragma_All_Calls_Remote              54
#define  Pragma_Annotate                      55
#define  Pragma_Assert                        56
#define  Pragma_Asynchronous                  57
#define  Pragma_Atomic                        58
#define  Pragma_Atomic_Components             59
#define  Pragma_Attach_Handler                60
#define  Pragma_CIL_Constructor               61
#define  Pragma_Comment                       62
#define  Pragma_Common_Object                 63
#define  Pragma_Complete_Representation       64
#define  Pragma_Complex_Representation        65
#define  Pragma_Controlled                    66
#define  Pragma_Convention                    67
#define  Pragma_CPP_Class                     68
#define  Pragma_CPP_Constructor               69
#define  Pragma_CPP_Virtual                   70
#define  Pragma_CPP_Vtable                    71
#define  Pragma_Debug                         72
#define  Pragma_Elaborate                     73
#define  Pragma_Elaborate_All                 74
#define  Pragma_Elaborate_Body                75
#define  Pragma_Export                        76
#define  Pragma_Export_Exception              77
#define  Pragma_Export_Function               78
#define  Pragma_Export_Object                 79
#define  Pragma_Export_Procedure              80
#define  Pragma_Export_Value                  81
#define  Pragma_Export_Valued_Procedure       82
#define  Pragma_External                      83
#define  Pragma_Finalize_Storage_Only         84
#define  Pragma_Ident                         85
#define  Pragma_Import                        86
#define  Pragma_Import_Exception              87
#define  Pragma_Import_Function               88
#define  Pragma_Import_Object                 89
#define  Pragma_Import_Procedure              90
#define  Pragma_Import_Valued_Procedure       91
#define  Pragma_Inline                        92
#define  Pragma_Inline_Always                 93
#define  Pragma_Inline_Generic                94
#define  Pragma_Inspection_Point              95
#define  Pragma_Interface_Name                96
#define  Pragma_Interrupt_Handler             97
#define  Pragma_Interrupt_Priority            98
#define  Pragma_Java_Constructor              99
#define  Pragma_Java_Interface                100
#define  Pragma_Keep_Names                    101
#define  Pragma_Link_With                     102
#define  Pragma_Linker_Alias                  103
#define  Pragma_Linker_Constructor            104
#define  Pragma_Linker_Destructor             105
#define  Pragma_Linker_Options                106
#define  Pragma_Linker_Section                107
#define  Pragma_List                          108
#define  Pragma_Machine_Attribute             109
#define  Pragma_Main                          110
#define  Pragma_Main_Storage                  111
#define  Pragma_Memory_Size                   112
#define  Pragma_No_Body                       113
#define  Pragma_No_Return                     114
#define  Pragma_Obsolescent                   115
#define  Pragma_Optimize                      116
#define  Pragma_Pack                          117
#define  Pragma_Page                          118
#define  Pragma_Passive                       119
#define  Pragma_Preelaborable_Initialization  120
#define  Pragma_Preelaborate                  121
#define  Pragma_Preelaborate_05               122
#define  Pragma_Psect_Object                  123
#define  Pragma_Pure                          124
#define  Pragma_Pure_05                       125
#define  Pragma_Pure_Function                 126
#define  Pragma_Remote_Call_Interface         127
#define  Pragma_Remote_Types                  128
#define  Pragma_Share_Generic                 129
#define  Pragma_Shared                        130
#define  Pragma_Shared_Passive                131
#define  Pragma_Source_Reference              132
#define  Pragma_Static_Elaboration_Desired    133
#define  Pragma_Stream_Convert                134
#define  Pragma_Subtitle                      135
#define  Pragma_Suppress_All                  136
#define  Pragma_Suppress_Debug_Info           137
#define  Pragma_Suppress_Initialization       138
#define  Pragma_System_Name                   139
#define  Pragma_Task_Info                     140
#define  Pragma_Task_Name                     141
#define  Pragma_Task_Storage                  142
#define  Pragma_Time_Slice                    143
#define  Pragma_Title                         144
#define  Pragma_Unchecked_Union               145
#define  Pragma_Unimplemented_Unit            146
#define  Pragma_Universal_Aliasing            147
#define  Pragma_Unreferenced                  148
#define  Pragma_Unreferenced_Objects          149
#define  Pragma_Unreserve_All_Interrupts      150
#define  Pragma_Volatile                      151
#define  Pragma_Volatile_Components           152
#define  Pragma_Weak_External                 153
#define  Pragma_AST_Entry                     154
#define  Pragma_Interface                     155
#define  Pragma_Priority                      156
#define  Pragma_Storage_Size                  157
#define  Pragma_Storage_Unit                  158

/* End of snames.h (C version of Snames package spec) */
