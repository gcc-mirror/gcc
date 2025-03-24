/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vcmpbf16" 10 } } */

typedef __bf16 v16bf __attribute__ ((__vector_size__ (32)));
typedef __bf16 v8bf __attribute__ ((__vector_size__ (16)));

#define VCMPMN(type, op, name)	\
type  \
__attribute__ ((noinline, noclone)) \
vec_cmp_##type##type##name (type a, type b) \
{ \
  return a op b;  \
}

VCMPMN (v16bf, <, lt)
VCMPMN (v8bf, <, lt)

VCMPMN (v16bf, <=, le)
VCMPMN (v8bf, <=, le)

VCMPMN (v16bf, >, gt)
VCMPMN (v8bf, >, gt)

VCMPMN (v16bf, >=, ge)
VCMPMN (v8bf, >=, ge)

VCMPMN (v16bf, ==, eq)
VCMPMN (v8bf, ==, eq)
