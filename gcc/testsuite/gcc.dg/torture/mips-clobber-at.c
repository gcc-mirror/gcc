/* "$1" used to be mapped to the internal frame pointer.  */
/* { dg-do compile { target mips*-*-* } } */
/* { dg-options "" } */
int foo () { asm volatile ("#" ::: "$1"); }
