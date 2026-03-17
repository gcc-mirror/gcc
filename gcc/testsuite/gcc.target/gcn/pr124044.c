/* { dg-do compile }
   { dg-options "-O -Wall" } */
/* PR target/124044  */

void *
foo (void __flat* ptr)
{ return ptr; }

void __flat *
foo2 (void* ptr)
{ return ptr; }

/* This test relies on flat addressing being used for pointers without a
   specified address space.  */
/* { dg-final { scan-assembler-not "using global addressing" } }
   { dg-final { scan-assembler-times "using flat addressing" 2 } } */
