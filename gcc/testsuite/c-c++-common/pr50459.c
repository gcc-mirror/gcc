/* PR c/50459 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wextra" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

enum { A = 128, B = 1 };
void *fn1 (void) __attribute__((assume_aligned (A)));
void *fn2 (void) __attribute__((assume_aligned (A, 4)));
void *fn5 (int) __attribute__((alloc_size (B)));
void *fn6 (int) __attribute__((alloc_align (B)));
void fn7 (const char *, ...) __attribute__ ((sentinel (B)));
int __attribute__((vector_size (A))) a;
int __attribute__((aligned (A))) foo;
