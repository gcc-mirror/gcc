/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

/* { dg-final { scan-assembler-times "vcmpph" 45 } } */
/* { dg-final { scan-assembler-times "vpcmpuw" 12 } } */
/* { dg-final { scan-assembler-times "vpcmpw" 18 } } */
/* { dg-final { scan-assembler-times "(?:vpblendmw|vmovdqu16\[^\{\n\]+\{%k\[1-7\]\})" 75 } } */

typedef _Float16 v8hf __attribute__ ((vector_size (16)));
typedef _Float16 v16hf __attribute__ ((vector_size (32)));
typedef _Float16 v32hf __attribute__ ((vector_size (64)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef short v32hi __attribute__ ((vector_size (64)));
typedef unsigned short v8uhi __attribute__ ((vector_size (16)));
typedef unsigned short v16uhi __attribute__ ((vector_size (32)));
typedef unsigned short v32uhi __attribute__ ((vector_size (64)));

#define VCONDMOV(size, op, name)  \
v##size##hf \
__attribute__ ((noinline, noclone)) \
vcond_##v##size##hf##v##size##hf##name (v##size##hf a, v##size##hf b,  \
			   v##size##hf c, v##size##hf d)  \
{ \
  return (a op b) ? c : d;  \
}\
v##size##hf \
__attribute__ ((noinline, noclone)) \
vcond_##v##size##hi##v##size##hf##name (v##size##hi a, v##size##hi b,  \
			   v##size##hf c, v##size##hf d)  \
{ \
  return (a op b) ? c : d;  \
}\
v##size##hi \
__attribute__ ((noinline, noclone)) \
vcond_##v##size##hf##v##size##hi##name (v##size##hi a, v##size##hi b,  \
			   v##size##hf c, v##size##hf d)  \
{ \
  return (c op d) ? a : b;  \
} \
v##size##hf \
__attribute__ ((noinline, noclone)) \
vcond_##v##size##uhi##v##size##hf##name (v##size##uhi a, v##size##uhi b,  \
			   v##size##hf c, v##size##hf d)  \
{ \
  return (a op b) ? c : d;  \
}\
v##size##uhi \
__attribute__ ((noinline, noclone)) \
vcond_##v##size##hf##v##size##uhi##name (v##size##uhi a, v##size##uhi b,  \
			   v##size##hf c, v##size##hf d)  \
{ \
  return (c op d) ? a : b;  \
} \

VCONDMOV (8, <, lt)
VCONDMOV (8, >, gt)
VCONDMOV (8, ==, eq)
VCONDMOV (8, <=, le)
VCONDMOV (8, >=, ge)
VCONDMOV (16, <, lt)
VCONDMOV (16, >, gt)
VCONDMOV (16, <=, le)
VCONDMOV (16, >=, ge)
VCONDMOV (16, ==, eq)
VCONDMOV (32, <, lt)
VCONDMOV (32, >, gt)
VCONDMOV (32, <=, le)
VCONDMOV (32, >=, ge)
VCONDMOV (32, ==, eq)
