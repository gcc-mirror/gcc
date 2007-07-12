/* { dg-do compile } */
/* { dg-options "-mcpu=cpu32" } */

/* Check that interrupt_thread is rejected on CPUs other than
   fido.  */

extern void foo (void) __attribute__((interrupt_thread)); /* { dg-error "interrupt_thread is available only on fido" } */
