/* PR optimization/11304 */
/* Originator: <manuel.serrano@sophia.inria.fr> */
/* { dg-do run } */
/* { dg-options "-O -fomit-frame-pointer" } */

/* Verify that %eax is always restored after a call.  */

__attribute__((noipa)) void set_eax(int val);
__attribute__((noipa)) void foo(int val);
__attribute__((noipa)) int bar(int x);

#include "call-1.c"
