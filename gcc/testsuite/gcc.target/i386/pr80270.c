/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

typedef int v8 __attribute__((vector_size(8)));
struct S1 {
  v8 s1f;
};
struct S2 {
  struct S1 s2f1;
  v8 s2f2;
};

extern void foo(int);

void bar() {
  int tmp, i = 3;
  register struct S2 b asm("xmm0");
  tmp = b.s2f1.s1f[i];
  foo(tmp);
}

