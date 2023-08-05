/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re" } */

#include "core-support.h"

extern int *v;

int foo(void *data)
{
  int i = 0;

  v[i++] = bpf_core_type_id_local (struct { int a; });
  v[i++] = bpf_core_type_id_local (union { int a; });
  v[i++] = bpf_core_type_id_local (enum { A = 1 });
  v[i++] = bpf_core_type_id_local (char(*)(int));
  v[i++] = bpf_core_type_id_local (void *);
  v[i++] = bpf_core_type_id_local (char[10]);

  v[i++] = bpf_core_type_id_local (struct my_struct);
  v[i++] = bpf_core_type_id_local (union my_union);
  v[i++] = bpf_core_type_id_local (enum my_enum);
  v[i++] = bpf_core_type_id_local (int);
  v[i++] = bpf_core_type_id_local (named_struct_tdef);
  v[i++] = bpf_core_type_id_local (func_tdef);
  v[i++] = bpf_core_type_id_local (array_tdef);

  v[i++] = bpf_core_type_id_target (struct my_struct);
  v[i++] = bpf_core_type_id_target (union my_union);
  v[i++] = bpf_core_type_id_target (enum my_enum);
  v[i++] = bpf_core_type_id_target (int);
  v[i++] = bpf_core_type_id_target (named_struct_tdef);
  v[i++] = bpf_core_type_id_target (func_tdef);
  v[i++] = bpf_core_type_id_target (array_tdef);

 return 0;
}

/* { dg-final { scan-assembler-times "\t.4byte\t0\t; bpfcr_type" 0  { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times "\t.4byte\t0x6\t; bpfcr_kind" 13 } } BPF_TYPE_ID_LOCAL */
/* { dg-final { scan-assembler-times "\t.4byte\t0x7\t; bpfcr_kind" 7 } } BPF_TYPE_ID_TARGET */
