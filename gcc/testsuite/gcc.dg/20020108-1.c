/* This testcase failed on i686 because (const_int -1) was changed into
   (const_int 0xffff) when storing it into SImode pseudo, but was not
   converted back to (const_int -1) when returning from store_expr,
   eventhough target was (subreg:HI (reg/v:SI indx)).  But (const_int 0xffff)
   is not valid general_operand in HImode.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -mtune=i686" { target i?86-*-* } } */

void
foo (unsigned short *cp)
{
  unsigned short indx;

  *cp = indx = 0xFFFF;
}
