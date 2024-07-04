/* { dg-do compile } */
/* { dg-options "-O2 -dA -gbtf -mco-re -masm=normal" } */

#include "core-support.h"

extern int *v;

unsigned long foo(void *data)
{
 enum named_ue64 named_unsigned = 0;
 enum named_se64 named_signed = 0;
 int i = 0;

 v[i++] = bpf_core_enum_value_exists (named_unsigned, UE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_ue64, UE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_ue64, UE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (named_signed, SE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_se64,	SE64_VAL1);
 v[i++] = bpf_core_enum_value_exists (enum named_se64,	SE64_VAL1);

 v[i++] = bpf_core_enum_value (named_unsigned, UE64_VAL1);
 v[i++] = bpf_core_enum_value (named_unsigned, UE64_VAL1);
 v[i++] = bpf_core_enum_value (named_signed, SE64_VAL1);
 v[i++] = bpf_core_enum_value (named_signed, SE64_VAL1);

 return 0;
}

/* { dg-final { scan-assembler-times "bpfcr_type \\(named_ue64\\)" 2 } } */
/* { dg-final { scan-assembler-times "bpfcr_type \\(named_se64\\)" 2} } */
/* { dg-final { scan-assembler-times "0xa\[\t \]+\[^\n\]*bpfcr_kind" 2 } } BPF_ENUMVAL_EXISTS */
/* { dg-final { scan-assembler-times "0xb\[\t \]+\[^\n\]*bpfcr_kind" 2 } } BPF_ENUMVAL_VALUE */

/* { dg-final { scan-assembler-times "bpfcr_astr_off \\(\"0\"\\)" 4 } } */

