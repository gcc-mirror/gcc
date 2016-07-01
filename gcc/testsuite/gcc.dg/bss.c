/* Test non-zero initializers in .bss-like sections get properly refused.  */
/* { dg-do compile } */
/* { dg-require-named-sections "" } */

int __attribute__((section(".bss.local"))) x = 1; /* { dg-error "" "zero init" } */
int *__attribute__((section(".bss.local"))) px = &x; /* { dg-error "" "zero init" } */
int __attribute__((section(".bss.local"))) y = 0;
int *__attribute__((section(".bss.local"))) py = (void*)0;
