/* { dg-do compile } */
/* { dg-additional-options "-mcmse -g" } */
typedef int __attribute__((cmse_nonsecure_call)) (*t)();
t f;
void g() { f(); }
