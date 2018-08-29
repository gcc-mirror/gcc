/* { dg-options "-Wdeprecated-declarations" } */

/* Check that MSG is printed without the escape characters being interpreted.
   Especially the newlines.

   Note - gcc's behaviour is inconsistent in this regard as #error and
   #warning will also display control characters as escape sequences,
   whereas #pragma GCC error and #pragma GCC warning will perform the
   control operations of the control characters.  */
   
#define MSG "foo\n\t\rbar"

int f (int i __attribute__ ((deprecated (MSG))))
{
  return 0 ? i : 0; /* { dg-warning "'i' is deprecated: foo.n.t.rbar" } */
}

