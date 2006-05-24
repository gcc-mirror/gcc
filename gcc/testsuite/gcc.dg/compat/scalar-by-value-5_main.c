/* Test that function args can be passed in various positions to both fixed
   and variable arg functions.  */
/* { dg-options "-O" } */

extern void exit (int);
extern void longlong_i_doit (void);
extern void longlong_d_doit (void);
extern void complexint_i_doit (void);
extern void complexint_d_doit (void);
extern void complexdouble_i_doit (void);
extern void complexdouble_d_doit (void);
extern void complexlonglong_i_doit (void);
extern void complexlonglong_d_doit (void);

int main (void)
{
  longlong_i_doit ();
  longlong_d_doit ();
#ifndef SKIP_COMPLEX_INT
  complexint_i_doit ();
  complexint_d_doit ();
#endif
  complexdouble_i_doit ();
  complexdouble_d_doit ();
#ifndef SKIP_COMPLEX_INT
  complexlonglong_i_doit ();
  complexlonglong_d_doit ();
#endif
  exit (0);
}
