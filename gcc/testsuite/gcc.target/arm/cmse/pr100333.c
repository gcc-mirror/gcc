/* { dg-do compile } */
/* { dg-additional-options "-mcmse" } */
typedef void __attribute__((cmse_nonsecure_call)) t(void);
t g;
void f() {
  g();
}
