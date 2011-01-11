/* { dg-do compile } */

__inline void foo(void) __attribute__((weak));  /* { dg-warning "inline.*weak" } */

