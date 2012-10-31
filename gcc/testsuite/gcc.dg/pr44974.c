/* PR middle-end/44974 */
/* { dg-do compile } */
/* { dg-options "-O -fno-optimize-sibling-calls" } */

extern void foo (int status) __attribute__ ((__noreturn__,__noinline__));
extern void bar (int status) __attribute__ ((__noreturn__,__noinline__));
extern void _Exit (int status) __attribute__ ((__noreturn__,__noinline__));

void
foo (int status)
{
  _Exit (status);
}

void
_Exit (int status)
{
  bar (status);
}

/* { dg-final { scan-assembler "call\[^\n\]*_Exit" { target i?86-*-* x86_64-*-* ia64-*-* sparc*-*-* } } } */
/* { dg-final { scan-assembler "bl\[^\n\]*_Exit" { target powerpc*-*-* } } } */
/* { dg-final { scan-assembler "brasl\[^\n\]*_Exit" { target { s390*-*-* && lp64 } } } } */
