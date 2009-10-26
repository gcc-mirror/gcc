/* { dg-do run { xfail rx-*-* } } */
/* { dg-skip-if "skipped until patch for generic zero=width bit-field handling is accepted" { rx-*-* } { "*" } { "" } } */
/* { dg-options "-msim" } */
/* Note: The -msim abiove is actually there to override the default
   options which do not allow the GCC extension of zero-width bitfields.  */

extern void abort (void);
extern void exit  (int);

struct S_zero
{
  int   f1: 4;
  int   f2: 0;
  short f3: 4;
} S_zero;

struct S_norm
{
  int   f1: 4;
  short f3: 4;
} S_norm;
 
 
int
main (void)
{
  if (sizeof (S_zero) != 4 || sizeof (S_norm) != 8)
    abort ();

  exit (0);
  return 0;
}
