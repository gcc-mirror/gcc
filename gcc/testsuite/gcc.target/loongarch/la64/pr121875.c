/* { dg-do compile } */
/* { dg-options "-march=loongarch64 -mabi=lp64d -O2" } */

[[gnu::always_inline]] inline void f() {}
[[gnu::target("lasx")]] int main() {f();}
