/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-fomit-frame-pointer" } */

void foo()
{
  int a=0, b=0, c=0, e=0, f=0, g=0, h=0, i=0;
    __asm__ __volatile__ (""
    :
    :
    : "bp"
  );
}

