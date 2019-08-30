/* PR optimization/11304 */
/* Originator: <manuel.serrano@sophia.inria.fr> */
/* { dg-do run } */
/* { dg-options "-O -fomit-frame-pointer" } */

/* Verify that %eax is always restored after a call.  */

__attribute__((noinline, noclone)) void set_eax(int val);
__attribute__((noinline, noclone)) void foo(int val);
__attribute__((noinline, noclone)) int bar(int x);

#include "call-1.c"
