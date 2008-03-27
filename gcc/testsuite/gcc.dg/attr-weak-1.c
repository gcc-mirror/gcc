/* { dg-do compile } */

__inline void foo(void) __attribute__((weak));  /* { dg-error "inline.*weak" } */

