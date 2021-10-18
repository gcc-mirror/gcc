/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

/* { dg-final { scan-assembler-times "vcmpph" 15 } } */

typedef _Float16 v8hf __attribute__ ((vector_size (16)));
typedef _Float16 v16hf __attribute__ ((vector_size (32)));
typedef _Float16 v32hf __attribute__ ((vector_size (64)));

#define VCMPMN(type, op, name)	\
type  \
__attribute__ ((noinline, noclone)) \
vec_cmp_##type##type##name (type a, type b) \
{ \
  return a op b;  \
}

VCMPMN (v8hf, <, lt)
VCMPMN (v16hf, <, lt)
VCMPMN (v32hf, <, lt)
VCMPMN (v8hf, <=, le)
VCMPMN (v16hf, <=, le)
VCMPMN (v32hf, <=, le)
VCMPMN (v8hf, >, gt)
VCMPMN (v16hf, >, gt)
VCMPMN (v32hf, >, gt)
VCMPMN (v8hf, >=, ge)
VCMPMN (v16hf, >=, ge)
VCMPMN (v32hf, >=, ge)
VCMPMN (v8hf, ==, eq)
VCMPMN (v16hf, ==, eq)
VCMPMN (v32hf, ==, eq)
