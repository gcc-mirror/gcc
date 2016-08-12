// { dg-options "-gdwarf-4 -gstrict-dwarf -dA" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param" 1 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param\[^\n\]*\n\[^\n\]* DW_AT_name\n\[^\n\]* DW_AT_type\n\[^\n\]* \[^\n\]*DIE" 1 } }
#include "template-params-12.H"
/* We do NOT get a value or location for this one, because we've
   enabled strict DWARF 4, and it could only be emitted as a location,
   which is DWARF 5 only for template value params.  */
template void t<&S::f>();
