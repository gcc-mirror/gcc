/* { dg-do compile  { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-Wall" } */

register int * volatile x asm ("ebx"); /* { dg-warning "optimization may eliminate reads and/or writes to register variables" } */
