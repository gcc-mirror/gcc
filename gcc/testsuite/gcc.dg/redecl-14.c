/* Some incompatible external linkage declarations were not diagnosed.
   Bug 21342.  Test type in inner scope is correct.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

typedef int IA[];
typedef int IA5[5];
typedef IA *IAP;
typedef IA5 *IA5P;
extern IAP a[];
void
f (void)
{
  {
    extern IA5P a[];
    sizeof (*a[0]);
  }
  extern IAP a[];
  extern IAP a[5];
  sizeof (*a[0]); /* { dg-error "error: invalid application of 'sizeof' to incomplete type 'IA'" } */
}
