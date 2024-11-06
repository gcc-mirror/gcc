/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -O2 -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times "vcmppbf16" 5 } } */

typedef __bf16 v32bf __attribute__ ((__vector_size__ (64)));

#define VCMPMN(type, op, name)	\
type  \
__attribute__ ((noinline, noclone)) \
vec_cmp_##type##type##name (type a, type b) \
{ \
  return a op b;  \
}

VCMPMN (v32bf, <, lt)
VCMPMN (v32bf, <=, le)
VCMPMN (v32bf, >, gt)
VCMPMN (v32bf, >=, ge)
VCMPMN (v32bf, ==, eq)
