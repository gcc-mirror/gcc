/* PR c/51628.  */
/* { dg-do compile } */
/* { dg-options "-O" } */

struct A
{
   int i;
};

struct B
{
   char c;
   __attribute ((packed)) struct A ar[4];
};

struct B b;

int *p = &b.ar[1].i;
/* { dg-warning "may result in an unaligned pointer value" "" { target *-*-* } .-1 } */
