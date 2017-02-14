// { dg-options "-gdwarf-2 -gno-strict-dwarf -dA" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param" 1 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param\[^\n\]*\n\[^\n\]* DW_AT_name\n\[^\n\]* DW_AT_type\n\[^\n\]*\[^\n\]* DW_AT_location\n\[^\n\]* DW_OP_addr\n\[^\n\]*_ZN1B1gEv\[^\n\]*\n\[^\n\]* DW_OP_stack_value\n\[^\n\]* DW_OP_piece\n\[^\n\]*\n\[^\n\]* DW_OP_lit0\n\[^\n\]* DW_OP_stack_value\n\[^\n\]* DW_OP_piece" 1 } }
#include "template-params-12.H"
/* We get a location list with a pair of DW_OP_pieces for pointers to
   non-virtual member functions.  */
template void t<&S::g>();
