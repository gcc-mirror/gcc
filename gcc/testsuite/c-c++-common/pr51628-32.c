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
   /* { dg-warning "attribute ignored" "" { target default_packed } .-1 } */
};

struct B b;

int *p = &b.ar[1].i;
/* { dg-warning "may result in an unaligned pointer value" "" { target { ! default_packed } } .-1 } */
