/* PR target/82370 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mno-avx2 -masm=att" } */
/* { dg-final { scan-assembler-times "vpslld\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsllq\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsllw\[ \t]\+\\\$7, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsrad\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsraq\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 0 } } */
/* { dg-final { scan-assembler-times "vpsraw\[ \t]\+\\\$3, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsrld\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsrlq\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */
/* { dg-final { scan-assembler-times "vpsrlw\[ \t]\+\\\$5, %xmm\[0-9]\+, %xmm\[0-9]\+" 3 } } */

typedef short int v32hi __attribute__((vector_size (64)));
typedef short int v16hi __attribute__((vector_size (32)));
typedef short int v8hi __attribute__((vector_size (16)));
typedef int v16si __attribute__((vector_size (64)));
typedef int v8si __attribute__((vector_size (32)));
typedef int v4si __attribute__((vector_size (16)));
typedef long long int v8di __attribute__((vector_size (64)));
typedef long long int v4di __attribute__((vector_size (32)));
typedef long long int v2di __attribute__((vector_size (16)));
typedef unsigned short int v32uhi __attribute__((vector_size (64)));
typedef unsigned short int v16uhi __attribute__((vector_size (32)));
typedef unsigned short int v8uhi __attribute__((vector_size (16)));
typedef unsigned int v16usi __attribute__((vector_size (64)));
typedef unsigned int v8usi __attribute__((vector_size (32)));
typedef unsigned int v4usi __attribute__((vector_size (16)));
typedef unsigned long long int v8udi __attribute__((vector_size (64)));
typedef unsigned long long int v4udi __attribute__((vector_size (32)));
typedef unsigned long long int v2udi __attribute__((vector_size (16)));

#ifdef __AVX512F__
v32hi f1 (v32hi *x) { return *x >> 3; }
v32uhi f2 (v32uhi *x) { return *x >> 5; }
v32uhi f3 (v32uhi *x) { return *x << 7; }
#endif
v16hi f4 (v16hi *x) { return *x >> 3; }
v16uhi f5 (v16uhi *x) { return *x >> 5; }
v16uhi f6 (v16uhi *x) { return *x << 7; }
v8hi f7 (v8hi *x) { return *x >> 3; }
v8uhi f8 (v8uhi *x) { return *x >> 5; }
v8uhi f9 (v8uhi *x) { return *x << 7; }
#ifdef __AVX512F__
v16si f10 (v16si *x) { return *x >> 3; }
v16usi f11 (v16usi *x) { return *x >> 5; }
v16usi f12 (v16usi *x) { return *x << 7; }
#endif
v8si f13 (v8si *x) { return *x >> 3; }
v8usi f14 (v8usi *x) { return *x >> 5; }
v8usi f15 (v8usi *x) { return *x << 7; }
v4si f16 (v4si *x) { return *x >> 3; }
v4usi f17 (v4usi *x) { return *x >> 5; }
v4usi f18 (v4usi *x) { return *x << 7; }
#ifdef __AVX512F__
v8di f19 (v8di *x) { return *x >> 3; }
v8udi f20 (v8udi *x) { return *x >> 5; }
v8udi f21 (v8udi *x) { return *x << 7; }
#endif
v4di f22 (v4di *x) { return *x >> 3; }
v4udi f23 (v4udi *x) { return *x >> 5; }
v4udi f24 (v4udi *x) { return *x << 7; }
v2di f25 (v2di *x) { return *x >> 3; }
v2udi f26 (v2udi *x) { return *x >> 5; }
v2udi f27 (v2udi *x) { return *x << 7; }
