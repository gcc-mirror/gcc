/* This caused an ICE during register spilling when targeting thumb.
   There are 8 registers available for arithmetic operations (r0-r7)
   r7 is the frame pointer, and r0-r3 are used to pass arguments.
   Combine was extending the lives of the arguments (in r0-r3) up until the
   call to z. This leaves only 3 regs free which isn't enough to preform the
   doubleword addition.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */
void z(int);
int foo(int a, int b, int c, int d, long long *q) 
{ 
  *q=*q+1; 
  z (a+b+c+d); 
}

