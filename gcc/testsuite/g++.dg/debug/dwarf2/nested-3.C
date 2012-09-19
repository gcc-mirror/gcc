// Origin: PR debug/45682
// { dg-options "-g -fno-merge-debug-strings -gdwarf-4 -dA -fdebug-types-section" }

namespace thread {

class Executor {
 public:
  static Executor* CurrentExecutor();
};

}

namespace thread {

Executor* Executor::CurrentExecutor() {
  return 0;
}

}

thread::Executor *te;

int
main ()
{
    return 0;
}

// We want to express the fact that the DIE for the definition of
// 'Executor::CurrentExecutor' is a grand-child of the DIE for the
// namespace 'thread'.  We must have something like this output:
//	.uleb128 0x8	# (DIE (0x29) DW_TAG_namespace)
//	.ascii "thread\0"	# DW_AT_name
//	.byte   0x1	# DW_AT_decl_file (.../testsuite/g++.dg/debug/dwarf2/nested-3.C)
//	.byte   0x4	# DW_AT_decl_line
//	.long   0x4b	# DW_AT_sibling
//	.uleb128 0x9	# (DIE (0x34) DW_TAG_class_type)
//	.long   .LASF0	# DW_AT_name: "Executor"
//			# DW_AT_declaration
//      .byte   0xa0    # DW_AT_signature
//      .byte   0xfe
//      .byte   0xe6
//      .byte   0x7b
//      .byte   0x66
//      .byte   0xe9
//      .byte   0x38
//      .byte   0xf0
//	.uleb128 0x5	# (DIE (0x39) DW_TAG_subprogram)
//			# DW_AT_external
//	.long   .LASF1	# DW_AT_name: "CurrentExecutor"
//	.byte   0x1	# DW_AT_decl_file (.../testsuite/g++.dg/debug/dwarf2/nested-3.C)
//	.byte   0x8	# DW_AT_decl_line
//	.long   .LASF2	# DW_AT_linkage_name: "_ZN6thread8Executor15CurrentExecutorEv"
//	.long   0x4b	# DW_AT_type
//	.byte   0x1	# DW_AT_accessibility
//			# DW_AT_declaration
//	.byte   0	# end of children of DIE 0x34
//	.byte   0	# end of children of DIE 0x29
//
//     Hence the scary regexp:
//
//     { dg-final { scan-assembler "\[^\n\r\]*\\(DIE \\(0x(\[0-9a-f\]+)\\) DW_TAG_namespace\\)\[\n\r\]+\[^\n\r\]*\"thread\[\^\n\r]+\[\n\r\]+(\[^\n\r\]*\[\n\r\]+)+\[^\n\r\]*\\(DIE \\(0x(\[0-9a-f\]+)\\) DW_TAG_class_type\\)(\[\n\r\]+\[^\n\r\]*)+\"Executor\[^\n\r\]+\[\n\r\]+\[^\n\r\]*DW_AT_declaration\[\n\r\]+\[^\n\r\]*DW_AT_signature\[^#;/!|@\]*\[#;/!|@\]+ \[^\n\r\]*\\(DIE\[^\n\r\]*DW_TAG_subprogram\\)\[\n\r\]+(\[^\n\r\]*\[\n\r\]+)+\[^\n\r\]*\"CurrentExecutor\[^\n\r\]+\[\n\r\]+(\[^\n\r\]*\[\n\r\]+)+(\[^\n\r\]*\[\n\r\]+)+\[^\n\r\]*end of children of DIE 0x\\3\[\n\r]+\[^\n\r\]*end of children of DIE 0x\\1\[\n\r]+" } }
