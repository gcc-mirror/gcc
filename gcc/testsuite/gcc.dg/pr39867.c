/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

int main (void)
{
  int exp = -1;
  /* Wrong folding of the LHS to an unsigned MAX leads to 4294967295 != 2.  */
  if ((exp < 2 ? 2U : (unsigned int) exp) != 2)
    link_error ();
  return 0;
}

