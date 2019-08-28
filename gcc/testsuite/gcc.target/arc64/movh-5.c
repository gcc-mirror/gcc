/* { dg-do compile } */
/* { dg-options "-O0 -fomit-frame-pointer" } */

/* check "movh-1.c" for further details. */

/* assign immediate to a memory: this immediate is small *
 * enough to be covered by w6 (signed 6 bit number).     */
short mem;
void foo(void)
{
  mem = 0x00;    /* the usual suspect: 0 */
  mem =  31;     /* largest positive number in w6 */
  mem = -32;     /* smallest negative number in w6 */
  mem = -1;      /* just a -1 (all bits 1) */
}
/* { dg-final { scan-assembler "sth\\s+0,\\\[" } } */
/* { dg-final { scan-assembler "sth\\s+31,\\\[" } } */
/* { dg-final { scan-assembler "sth\\s+-32,\\\[" } } */
/* { dg-final { scan-assembler "sth\\s+-1,\\\[" } } */
