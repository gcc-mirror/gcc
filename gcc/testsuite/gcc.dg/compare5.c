/* Test for a bogus warning on comparison between signed and unsigned.
   Origin: Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 8/21/2001.  */

/* { dg-do compile } */
/* { dg-options "-Wsign-compare" } */

extern void bar(void);

int foo(int x, int y, unsigned u)
{
  /* A *_DIV_EXPR is non-negative if both operands are.  */

  if (u < ((x=-22)/33)) /* { dg-warning "signed and unsigned" "DIV_EXPR" } */
    return x;

  if (u < ((x=22)/33))
    return x;

  if (u < ((x=22)/(y=33)))
    return x;

  if (u < (((x&0x10000)?128:64) / ((y&0x10000)?8:4)))
    return x;


  /* A *_MOD_EXPR is non-negative if the first operand is.  */

  if (u < ((x=-22)%33)) /* { dg-warning "signed and unsigned" "MOD_EXPR" } */
    return x;

  if (u < ((x=22)%-33))
    return x;

  if (u < ((x==y)%-33))
    return x;

  if (u < (((x=22)/33)%-33))
    return x;

  return 0;
}
