/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx10.2-256" } */
/* { dg-final { scan-assembler-times "vcmpbf16" 10 } } */

typedef __bf16 __attribute__((__vector_size__ (4))) v2bf;
typedef __bf16 __attribute__((__vector_size__ (8))) v4bf;


#define VCMPMN(type, op, name)	\
type  \
__attribute__ ((noinline, noclone)) \
vec_cmp_##type##type##name (type a, type b) \
{ \
  return a op b;  \
}

VCMPMN (v4bf, <, lt)
VCMPMN (v2bf, <, lt)
VCMPMN (v4bf, <=, le)
VCMPMN (v2bf, <=, le)
VCMPMN (v4bf, >, gt)
VCMPMN (v2bf, >, gt)
VCMPMN (v4bf, >=, ge)
VCMPMN (v2bf, >=, ge)
VCMPMN (v4bf, ==, eq)
VCMPMN (v2bf, ==, eq)
