/* { dg-options "-O2" } */

typedef __bf16 v8bf __attribute__((vector_size(16)));
typedef __bf16 v16bf __attribute__((vector_size(32)));
typedef __bf16 v64bf __attribute__((vector_size(128)));

v8bf f1(v8bf x) { return -x; }
v16bf f2(v16bf x) { return -x; }
v64bf f3(v64bf x) { return -x; }

/* { dg-final { scan-assembler-times {\teor\t[zv]} 11 } } */
