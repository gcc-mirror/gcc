/* PR middle-end/15486 */
/* Origin: Jonathan Larmour <jifl-bugzilla@jifvik.org> */

/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-fdata-sections" } */

int x;

/* { dg-final { scan-assembler "comm" } } */
