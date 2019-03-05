/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-mcpu=msp430" } { "" } } */
/* { dg-options "-mcpu=msp430x" } */

typedef _Complex __int20 C;

C
foo (C x, C y)
{
  return x + y;
}
