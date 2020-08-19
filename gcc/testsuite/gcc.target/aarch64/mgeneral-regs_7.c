/* { dg-options "-mgeneral-regs-only" } */

typedef int v4si __attribute__ ((__vector_size__ ((16))));

v4si (*foo) ();

void
f0 (v4si *ptr)
{
  *ptr = foo (); /* { dg-error "'-mgeneral-regs-only' is incompatible with the use of vector types" } */
}
