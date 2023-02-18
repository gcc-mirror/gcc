/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int __attribute__((returns_twice)) setjmp(void*);

void bbb(void) {
  int (*fnptr)(void*) = setjmp;
  fnptr(0);
}
