/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mno-accumulate-outgoing-args -mpreferred-stack-boundary=2" } */

typedef struct { long long w64[2]; } V128;
extern V128* fn2(void);
long long a;
V128 b;
void fn1() {
  V128 *c = fn2();
  c->w64[0] = a ^ b.w64[0];
}
