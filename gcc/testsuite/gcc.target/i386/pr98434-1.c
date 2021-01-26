/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2 -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times {vpsravw[\t ]*%xmm} 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times {vpsrlvw[\t ]*%ymm} 2 } } */
/* { dg-final { scan-assembler-times {vpsllvw[\t ]*%zmm} 2 } } */
/* { dg-final { scan-assembler-times {vpsllvq[\t ]*%xmm} 1 } } */
/* { dg-final { scan-assembler-times {vpsravq[\t ]*%ymm} 1 } } */
/* { dg-final { scan-assembler-times {vpsrlvq[\t ]*%zmm} 1 } } */

int n;

typedef char v8qi __attribute__((vector_size (8)));
typedef char v16qi __attribute__((vector_size (16)));
typedef char v32qi __attribute__((vector_size (32)));
typedef short v8hi __attribute__((vector_size (16)));
typedef short v16hi __attribute__((vector_size (32)));
typedef short v32hi __attribute__((vector_size (64)));
typedef long long v2di __attribute__((vector_size (16)));
typedef long long v4di __attribute__((vector_size (32)));
typedef long long v8di __attribute__((vector_size (64)));
typedef unsigned char v8uqi __attribute__((vector_size (8)));
typedef unsigned char v16uqi __attribute__((vector_size (16)));
typedef unsigned char v32uqi __attribute__((vector_size (32)));
typedef unsigned short v8uhi __attribute__((vector_size (16)));
typedef unsigned short v16uhi __attribute__((vector_size (32)));
typedef unsigned short v32uhi __attribute__((vector_size (64)));
typedef unsigned long long v2udi __attribute__((vector_size (16)));
typedef unsigned long long v4udi __attribute__((vector_size (32)));
typedef unsigned long long v8udi __attribute__((vector_size (64)));

#define FOO(TYPE, OP, NAME)		\
  __attribute__((noipa)) TYPE		\
  foo_##TYPE##_##NAME (TYPE a, TYPE b)	\
  {					\
    return a OP b;			\
  }					\

FOO (v8qi, <<, vashl);
FOO (v8qi, >>, vashr);
FOO (v8uqi, >>, vlshr);
FOO (v16qi, <<, vashl);
FOO (v16qi, >>, vashr);
FOO (v16uqi, >>, vlshr);
FOO (v32qi, <<, vashl);
FOO (v32qi, >>, vashr);
FOO (v32uqi, >>, vlshr);
FOO (v8hi, <<, vashl);
FOO (v8hi, >>, vashr);
FOO (v8uhi, >>, vlshr);
FOO (v16hi, <<, vashl);
FOO (v16hi, >>, vashr);
FOO (v16uhi, >>, vlshr);
FOO (v32hi, <<, vashl);
FOO (v32hi, >>, vashr);
FOO (v32uhi, >>, vlshr);
FOO (v2di, <<, vashl);
FOO (v2di, >>, vashr);
FOO (v2udi, >>, vlshr);
FOO (v4di, <<, vashl);
FOO (v4di, >>, vashr);
FOO (v4udi, >>, vlshr);
FOO (v8di, <<, vashl);
FOO (v8di, >>, vashr);
FOO (v8udi, >>, vlshr);
