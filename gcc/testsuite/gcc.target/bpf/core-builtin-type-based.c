/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

#include "core-support.h"

extern int *v;

int foo(void *data)
{
  int i = 0;

  v[i++] = bpf_core_type_exists (struct my_struct);
  v[i++] = bpf_core_type_exists (struct my_complex_struct);
  v[i++] = bpf_core_type_exists (union my_union);
  v[i++] = bpf_core_type_exists (enum my_enum);
  v[i++] = bpf_core_type_exists (named_struct_tdef);
  v[i++] = bpf_core_type_exists (anon_struct_tdef);
  v[i++] = bpf_core_type_exists (struct_ptr_tdef);
  v[i++] = bpf_core_type_exists (int_tdef);
  v[i++] = bpf_core_type_exists (enum_tdef);
  v[i++] = bpf_core_type_exists (void_ptr_tdef);
  v[i++] = bpf_core_type_exists (restrict_ptr_tdef);
  v[i++] = bpf_core_type_exists (func_tdef);
  v[i++] = bpf_core_type_exists (array_tdef);
 
  v[i++] = bpf_core_type_matches (struct my_struct);
  v[i++] = bpf_core_type_matches (struct my_complex_struct);
  v[i++] = bpf_core_type_matches (union my_union);
  v[i++] = bpf_core_type_matches (enum my_enum);
  v[i++] = bpf_core_type_matches (named_struct_tdef);
  v[i++] = bpf_core_type_matches (anon_struct_tdef);
  v[i++] = bpf_core_type_matches (struct_ptr_tdef);
  v[i++] = bpf_core_type_matches (int_tdef);
  v[i++] = bpf_core_type_matches (enum_tdef);
  v[i++] = bpf_core_type_matches (void_ptr_tdef);
  v[i++] = bpf_core_type_matches (restrict_ptr_tdef);
  v[i++] = bpf_core_type_matches (func_tdef);
  v[i++] = bpf_core_type_matches (array_tdef);
 
  v[i++] = bpf_core_type_size (struct my_struct);
  v[i++] = bpf_core_type_size (union my_union);
  v[i++] = bpf_core_type_size (enum my_enum);
  v[i++] = bpf_core_type_size (named_struct_tdef);
  v[i++] = bpf_core_type_size (anon_struct_tdef);
  v[i++] = bpf_core_type_size (struct_ptr_tdef);
  v[i++] = bpf_core_type_size (int_tdef);
  v[i++] = bpf_core_type_size (enum_tdef);
  v[i++] = bpf_core_type_size (void_ptr_tdef);
  v[i++] = bpf_core_type_size (func_tdef);
  v[i++] = bpf_core_type_size (array_tdef);

  return 0;
}

/* { dg-final { scan-assembler-times "0x0\[\t \]+\[^\n\]*bpfcr_type" 0 } } */
/* { dg-final { scan-assembler-times "0x8\[\t \]+\[^\n\]*bpfcr_kind" 13 } } BPF_TYPE_EXISTS */
/* { dg-final { scan-assembler-times "0x9\[\t \]+\[^\n\]*bpfcr_kind" 11 } } BPF_TYPE_SIZE */
/* { dg-final { scan-assembler-times "0xc\[\t \]+\[^\n\]*bpfcr_kind" 13 } } BPF_TYPE_MATCHES */
/* { dg-final { scan-assembler-times "bpfcr_astr_off \[(\"\]+0\[(\"\]+" 37 } } */
