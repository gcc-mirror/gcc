/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw" 1 } } */

typedef int v16si __attribute__((vector_size(64)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v4si __attribute__((vector_size(16)));

typedef short v32hi __attribute__((vector_size(64)));
typedef short v16hi __attribute__((vector_size(32)));
typedef short v8hi __attribute__((vector_size(16)));

typedef char v64qi __attribute__((vector_size(64)));
typedef char v32qi __attribute__((vector_size(32)));
typedef char v16qi __attribute__((vector_size(16)));

extern v16si sinksz;
extern v8si sinksy;
extern v4si sinksx;
extern v32hi sinkhz;
extern v16hi sinkhy;
extern v8hi sinkhx;
extern v64qi sinkbz;
extern v32qi sinkby;
extern v16qi sinkbx;

void foo(char c) {
  sinksz = __extension__(v16si){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
  sinksy = __extension__(v8si){c,c,c,c,c,c,c,c};
  sinksx = __extension__(v4si){c,c,c,c};
}

void foo1(char c) {
  sinkhz = __extension__(v32hi){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,
    c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
  sinkhy = __extension__(v16hi){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
  sinkhx = __extension__(v8hi){c,c,c,c,c,c,c,c};
}

void foo2(char c) {
  sinkbz = __extension__(v64qi){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,
    c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,
    c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,
    c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
  sinkby = __extension__(v32qi){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,
    c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
  sinkbx = __extension__(v16qi){c,c,c,c,c,c,c,c,c,c,c,c,c,c,c,c};
}
