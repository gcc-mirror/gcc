/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

#include "core-support.h"

extern int *v;

int foo(void *data)
{
 int i = 0;
 enum named_ue64 named_unsigned64 = 0;
 enum named_se64 named_signed64 = 0;
 enum named_ue named_unsigned = 0;
 enum named_se named_signed = 0;

 v[i++] = bpf_core_enum_value_exists (named_unsigned64, UE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_ue64, UE64_VAL2);
 v[i++] = bpf_core_enum_value_exists (enum named_ue64, UE64_VAL3);
 v[i++] = bpf_core_enum_value_exists (named_signed64, SE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_se64, SE64_VAL2);
 v[i++] = bpf_core_enum_value_exists (enum named_se64, SE64_VAL3);

 v[i++] = bpf_core_enum_value (named_unsigned64, UE64_VAL1);
 v[i++] = bpf_core_enum_value (named_unsigned64, UE64_VAL2);
 v[i++] = bpf_core_enum_value (named_signed64, SE64_VAL1);
 v[i++] = bpf_core_enum_value (named_signed64, SE64_VAL2);

 v[i++] = bpf_core_enum_value_exists (named_unsigned, UE_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_ue, UE_VAL2);
 v[i++] = bpf_core_enum_value_exists (enum named_ue, UE_VAL3);
 v[i++] = bpf_core_enum_value_exists (named_signed, SE_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_se, SE_VAL2);
 v[i++] = bpf_core_enum_value_exists (enum named_se, SE_VAL3);

 v[i++] = bpf_core_enum_value (named_unsigned, UE_VAL1);
 v[i++] = bpf_core_enum_value (named_unsigned, UE_VAL2);
 v[i++] = bpf_core_enum_value (named_signed, SE_VAL1);
 v[i++] = bpf_core_enum_value (named_signed, SE_VAL2);

 return 0;
}

/* { dg-final { scan-assembler-times "bpfcr_type \\(named_ue64\\)" 5 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(named_se64\\)" 5} } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(named_ue\\)" 5 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(named_se\\)" 5} } */
/* { dg-final { scan-assembler-times "0xa\[\t \]+\[^\n\]*bpfcr_kind" 12 } } BPF_ENUMVAL_EXISTS */
/* { dg-final { scan-assembler-times "0xb\[\t \]+\[^\n\]*bpfcr_kind" 8 } } BPF_ENUMVAL_VALUE */

/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0\"\\)" 8 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"1\"\\)" 8 } } */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"2\"\\)" 4 } } */
