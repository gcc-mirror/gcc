/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

/* { dg-final { scan-assembler-times "vminph" 3 } } */
/* { dg-final { scan-assembler-times "vmaxph" 3 } } */

typedef _Float16 v8hf __attribute__ ((vector_size (16)));
typedef _Float16 v16hf __attribute__ ((vector_size (32)));
typedef _Float16 v32hf __attribute__ ((vector_size (64)));

#define VCONDMINMAX(size, op, name)  \
v##size##hf \
__attribute__ ((noinline, noclone)) \
vminmax_##v##size##hf##v##size##hf##name (v##size##hf a, v##size##hf b)  \
{ \
  return (a op b) ? a : b;  \
}

VCONDMINMAX (8, <, min)
VCONDMINMAX (8, >, max)
VCONDMINMAX (16, <, min)
VCONDMINMAX (16, >, max)
VCONDMINMAX (32, <, min)
VCONDMINMAX (32, >, max)

