/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-mpreferred-stack-boundary=2" } */

/* This case is to detect a compile time regression introduced in stack
   branch development. */
f(){asm("%0"::"r"(1.5F));}g(){asm("%0"::"r"(1.5));}
