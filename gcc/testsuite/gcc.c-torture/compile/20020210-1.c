/* PR c/5615 */
/* { dg-xfail-if "regression/16417" { "*-*-*" } { "-O1" "-O2" "-O3 -fomit-frame-pointer" "-O3 -g" "-Os" } { "" } } */
void f(int a, struct {int b[a];} c) {}
