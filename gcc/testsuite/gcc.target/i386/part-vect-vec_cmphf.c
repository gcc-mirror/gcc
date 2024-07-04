/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vcmpph" 10 } } */

typedef _Float16 __attribute__((__vector_size__ (4))) v2hf;
typedef _Float16 __attribute__((__vector_size__ (8))) v4hf;


#define VCMPMN(type, op, name)	\
type  \
__attribute__ ((noinline, noclone)) \
vec_cmp_##type##type##name (type a, type b) \
{ \
  return a op b;  \
}

VCMPMN (v4hf, <, lt)
VCMPMN (v2hf, <, lt)
VCMPMN (v4hf, <=, le)
VCMPMN (v2hf, <=, le)
VCMPMN (v4hf, >, gt)
VCMPMN (v2hf, >, gt)
VCMPMN (v4hf, >=, ge)
VCMPMN (v2hf, >=, ge)
VCMPMN (v4hf, ==, eq)
VCMPMN (v2hf, ==, eq)
