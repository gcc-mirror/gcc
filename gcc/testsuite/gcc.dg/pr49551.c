/* { dg-do compile } */
/* { dg-options "-O -fdata-sections" } */

int x = 1;
int x;

/* { dg-final { scan-assembler-not {comm[\t ]+x} } } */
