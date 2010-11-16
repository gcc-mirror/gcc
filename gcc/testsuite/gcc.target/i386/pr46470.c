/* { dg-do compile } */
/* { dg-options "-Os -fomit-frame-pointer -fasynchronous-unwind-tables" } */
/* { dg-options "-Os -fomit-frame-pointer -mpreferred-stack-boundary=3 -fasynchronous-unwind-tables" { target ilp32 } } */

void f();
void g() { f(); f(); }

/* Both stack allocate and deallocate should be converted to push/pop.  */
/* { dg-final { scan-assembler-not "sp" } } */
