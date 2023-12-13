/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx -mno-avx2 -O2 " } */
/* { dg-final { scan-assembler-not {(?n)(%rsp)} } } */

typedef unsigned long long v4di __attribute((vector_size(32)));
typedef unsigned int v8si __attribute((vector_size(32)));
typedef unsigned short v16hi __attribute((vector_size(32)));
typedef unsigned char v32qi __attribute((vector_size(32)));

#define MASK  0x01010101
#define MASKL 0x0101010101010101ULL
#define MASKS 0x0101

v4di fooq() {
  return (v4di){MASKL,MASKL,MASKL,MASKL};
}

v8si food() {
  return (v8si){MASK,MASK,MASK,MASK,MASK,MASK,MASK,MASK};
}

v16hi foow() {
  return (v16hi){MASKS,MASKS,MASKS,MASKS,MASKS,MASKS,MASKS,MASKS,
    MASKS,MASKS,MASKS,MASKS,MASKS,MASKS,MASKS,MASKS};
}

v32qi foob() {
  return (v32qi){1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
}
