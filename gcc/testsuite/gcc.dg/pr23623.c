/* { dg-do compile } */
/* { dg-options "-fstrict-volatile-bitfields -fdump-rtl-final" } */

/* With -fstrict-volatile-bitfields, the volatile accesses to bf2.b
   and bf3.b must do unsigned int reads/writes.  The non-volatile
   accesses to bf1.b are not so constrained.  */

extern struct
{
  unsigned int b : 1;
  unsigned int : 31;
} bf1;

extern volatile struct
{
  unsigned int b : 1;
  unsigned int : 31;
} bf2;

extern struct
{
  volatile unsigned int b : 1;
  volatile unsigned int : 31;
} bf3;

void writeb(void)
{
  bf1.b = 1;
  bf2.b = 1;	/* volatile read + volatile write */
  bf3.b = 1;	/* volatile read + volatile write */
}

extern unsigned int x1, x2, x3;

void readb(void)
{
  x1 = bf1.b;
  x2 = bf2.b;   /* volatile write */
  x3 = bf3.b;   /* volatile write */
}

/* There should be 6 volatile MEMs total, but scan-rtl-dump-times counts
   the number of match variables and not the number of matches.  Since
   the parenthesized subexpression in the regexp introduces an extra match
   variable, we need to give a count of 12 instead of 6 here.  */
/* { dg-final { scan-rtl-dump-times "mem/v(/.)*:SI" 12 "final" } } */
/* { dg-final { cleanup-rtl-dump "final" } } */

