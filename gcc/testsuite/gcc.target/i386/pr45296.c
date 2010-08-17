/* { dg-do compile } */
/* { dg-options "" } */

register long double F80 asm("st"); /* { dg-error "stack register" } */
