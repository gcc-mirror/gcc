/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -std=gnu90" } */
register long s9 asm("s9"); /* { dg-note "conflicts with 's9'" } */
register long fp asm("fp"); /* { dg-warning "register of 'fp' used for multiple global register variables" } */
