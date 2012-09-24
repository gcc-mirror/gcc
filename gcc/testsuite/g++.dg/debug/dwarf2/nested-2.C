/*
  Origin: PR debug/45024
  { dg-options "-g -dA -fno-merge-debug-strings" }
  { dg-do compile }
*/

struct S {
  template<typename Z> struct T { };
};

S::T<int> tval;

/*
We want to express that the DIE of S::T<int> must be a child of the DIE of S, like in assembly this output:

	.uleb128 0x2	# (DIE (0x9e) DW_TAG_structure_type)
	.ascii "S\0"	# DW_AT_name
	.byte	0x1	# DW_AT_byte_size
	.byte	0x1	# DW_AT_decl_file (../../prtests/test-PR45024.cc)
	.byte	0x1	# DW_AT_decl_line
	.long	0xbc	# DW_AT_sibling
	.uleb128 0x3	# (DIE (0xa8) DW_TAG_structure_type)
	.ascii "T<int>\0"	# DW_AT_name
	.byte	0x1	# DW_AT_byte_size
	.byte	0x1	# DW_AT_decl_file (../../prtests/test-PR45024.cc)
	.byte	0x2	# DW_AT_decl_line
	.uleb128 0x4	# (DIE (0xb3) DW_TAG_template_type_param)
	.ascii "Z\0"	# DW_AT_name
	.long	0xbc	# DW_AT_type
	.byte	0	# end of children of DIE 0xa8
	.byte	0	# end of children of DIE 0x9e

Hence the slightly odd regexp:

  { dg-final { scan-assembler "\[^\n\r\]*\\(DIE\[^\n\r\]*DW_TAG_structure_type\\)\[\n\r\]+\[^\n\r\]*\"S\\\\0\"\[ \t\]+\(\[@|#;!\]+|//?\)\[ \t\]+DW_AT_name\[\n\r\]+\(.*\)?\\(DIE\[^\n\r\]*DW_TAG_structure_type\\)\[\n\r\]+\[^\n\r\]*\"T<int>\\\\0\"\[ \t\]+\(.*\)?\\(DIE\[^\n\r\]*DW_TAG_template_type_param\\)\[\n\r\]+\[^\n\r\]*\[\n\r\]+\[^\n\r\]*\[\n\r\]+\[^\n\r\]*\(\[@|#;!\]+|//?\)\[ \t\]+end of children of DIE\[^\n\r\]*\[\n\r\]+\[^\n\r\]*end of children of DIE\[^\n\r\]*" } }

 */
