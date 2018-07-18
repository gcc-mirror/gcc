/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-mpreferred-stack-boundary=2" } */

/* This case is to detect a compile time regression introduced in stack
   branch development. */
void f(){asm("%0"::"r"(1.5F));}void g(){asm("%0"::"r"(1.5));} /* { dg-warning "unsupported size" }  */
