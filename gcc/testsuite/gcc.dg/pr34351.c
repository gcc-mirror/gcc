/* { dg-do compile } */
/* { dg-options "-Wall" } */

register int * volatile x asm ("r13"); /* { dg-warning "optimization may eliminate reads and/or writes to register variables" } */
