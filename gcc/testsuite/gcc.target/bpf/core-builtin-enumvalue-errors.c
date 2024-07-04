/* { dg-do compile } */
/* { dg-options "-O0 -dA -gbtf -mco-re -masm=normal" } */

#include "core-support.h"

extern int *v;

unsigned long foo(void *data)
{
  int i = 0;
  enum named_ue64 named_unsigned = 0;
  enum named_se64 named_signed = 0;
  typeof(enum named_ue64) a = 0;

  v[i++] = __builtin_preserve_enum_value (({ extern typeof(named_unsigned) *_type0; _type0; }),	 0,	BPF_ENUMVAL_EXISTS); /* { dg-error "invalid enum value argument for enum value builtin" } */
  v[i++] = __builtin_preserve_enum_value (({ extern typeof(enum named_ue64) *_type0; _type0; }), v,	BPF_ENUMVAL_EXISTS); /* { dg-error "invalid enum value argument for enum value builtin" } */
  v[i++] = __builtin_preserve_enum_value (a,							 UE64_VAL3,	BPF_ENUMVAL_EXISTS); /* { dg-error "invalid type argument format for enum value builtin" } */
  v[i++] = __builtin_preserve_enum_value (UE64_VAL3); /* { dg-error "wrong number of arguments for enum value core builtin" } */


 return 0;
}
