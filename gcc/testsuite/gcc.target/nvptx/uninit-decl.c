/* { dg-do compile } */

int __attribute__ ((used)) common;
static int __attribute__ ((used)) local;

/* { dg-final { scan-assembler ".weak .global\[^,\n\r\]*common" } } */
/* { dg-final { scan-assembler "\[\n\r\].global\[^,\n\r\]*local" } } */
