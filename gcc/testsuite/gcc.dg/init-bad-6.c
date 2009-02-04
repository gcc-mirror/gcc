/* ICE arising from bug computing composite type of zero-length array
   types: PR 35433.  */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int* X;
typedef int* Y;

X (*p)[][0];
Y (*q)[][0];

typeof(*(0 ? p : q)) x = { 0 }; /* { dg-warning "excess elements in array initializer|near initialization" } */
