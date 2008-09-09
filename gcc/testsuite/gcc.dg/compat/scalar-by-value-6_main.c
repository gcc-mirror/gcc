/* Test that function args can be passed in various positions to both fixed
   and variable arg functions.  */
/* { dg-options "-O" } */
/* { dg-options "-O -mlong-double-128" { target powerpc*-*-* } } */

extern void exit (int);
extern void longdouble_i_doit (void);
extern void longdouble_d_doit (void);
#ifndef SKIP_COMPLEX
extern void complexlongdouble_i_doit (void);
extern void complexlongdouble_d_doit (void);
#endif

int main (void)
{
  longdouble_i_doit ();
  longdouble_d_doit ();
#ifndef SKIP_COMPLEX
  complexlongdouble_i_doit ();
  complexlongdouble_d_doit ();
#endif
  exit (0);
}
