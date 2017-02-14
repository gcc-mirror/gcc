// { dg-options "-gdwarf-2 -dA" }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param" 1 } }
// { dg-final { scan-assembler-times "DIE \\(\[^\n\]*\\) DW_TAG_template_value_param\[^\n\]*\n\[^\n\]* DW_AT_name\n\[^\n\]* DW_AT_type\n\[^\n\]* DW_AT_const_value" 1 } }
#include "template-params-12.H"
/* We get const_value for NULL pointers to member functions.  */
#if __cplusplus > 199711L // Ugh, C++98 barfs at both the cast and the overload.
template void t<static_cast<void (S::*)()>(0)>();
#else
template void t<&S::v>();
#endif
