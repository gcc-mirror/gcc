// { dg-options "-gdwarf-2 -dA" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param" 1 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param\[^\n\]*\n\[^\n\]* DW_AT_name\n\[^\n\]* DW_AT_type\n\[^\n\]* \[^\n\]*DIE" 1 } }
#include "template-params-12.H"
/* We do NOT get a value or location for this one, because it would
   require a relocation to an undefined symbol in a debug section.  */
template void t<&S::u>();
