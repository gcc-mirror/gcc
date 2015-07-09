// Origin PR debug/46955
// { dg-options "-gdwarf-2 -dA" }
// { dg-do compile }

struct S { int f; };
template<int S::*MP> struct T { };
T<&S::f> v;

// For the type of v, we should have this DWARF generated:
//	.uleb128 0x6	# (DIE (0x57) DW_TAG_template_value_param)
//	.ascii "MP\0"	# DW_AT_name
//	.long	0x61	# DW_AT_type
//	.byte	0	# DW_AT_const_value
// So let's look for that.
// { dg-final { scan-assembler-times "\[^\n\r\]*DIE \\(\[^\n\r\]*\\) DW_TAG_template_value_param\[^\n\r\]*\[\n\r\]{1,2}\[^\n\r\]*DW_AT_name\[\n\r\]{1,2}\[^\n\r\]*DW_AT_type\[\n\r\]{1,2}\[^\n\r\]*DW_AT_const_value\[\n\r\]{1,2}" 1 } }
