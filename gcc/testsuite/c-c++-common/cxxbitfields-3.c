/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 --param allow-store-data-races=0" } */

/* Make sure we don't narrow down to a QI or HI to store into VAR.J,
   but instead use an SI.  */

struct S
{ 
  volatile int i: 4;
  volatile int j: 4;
  volatile int k: 8;
  volatile int l: 8;
  volatile int m: 8;
} var;

void setit()
{ 
  var.j = 5;
}

/* { dg-final { scan-assembler "movl.*, _?var" { target nonpic } } } */
/* { dg-final { scan-assembler "movl.*, (_?var|\\(%)" { target { ! nonpic } } } } */
