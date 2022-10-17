/* Test that we can handle leading-zeros on mmacosx-version-min for invocations
   including linking (so that spec processing works).  To make sure that any
   necessary target libs are present we make this specific to the target version
   being built.  */
/* { dg-do link { target *-*-darwin* } } */
/* { dg-additional-options "-mmacosx-version-min=010.04.11 -DCHECK=1049" { target *-*-darwin8* } } */
/* { dg-additional-options "-mmacosx-version-min=010.05.08 -DCHECK=1058" { target *-*-darwin9* } } */
/* { dg-additional-options "-mmacosx-version-min=010.06.08 -DCHECK=1068" { target *-*-darwin10* } } */
/* { dg-additional-options "-mmacosx-version-min=010.07.05 -DCHECK=1075" { target *-*-darwin11* } } */
/* { dg-additional-options "-mmacosx-version-min=010.08.05 -DCHECK=1085" { target *-*-darwin12* } } */
/* { dg-additional-options "-mmacosx-version-min=010.09.05 -DCHECK=1095" { target *-*-darwin13* } } */
/* { dg-additional-options "-mmacosx-version-min=010.010.03 -DCHECK=101003" { target *-*-darwin14* } } */
/* { dg-additional-options "-mmacosx-version-min=010.011.06 -DCHECK=101106" { target *-*-darwin15* } } */
/* { dg-additional-options "-mmacosx-version-min=010.012.06 -DCHECK=101206" { target *-*-darwin16* } } */
/* { dg-additional-options "-mmacosx-version-min=010.013.06 -DCHECK=101306" { target *-*-darwin17* } } */
/* { dg-additional-options "-mmacosx-version-min=010.014.05 -DCHECK=101405" { target *-*-darwin18* } } */
/* { dg-additional-options "-mmacosx-version-min=010.015.06 -DCHECK=101506" { target *-*-darwin19* } } */
/* { dg-additional-options "-mmacosx-version-min=011.000.00 -DCHECK=110000" { target *-*-darwin20* } } */
/* { dg-additional-options "-mmacosx-version-min=012.000.00 -DCHECK=120000" { target *-*-darwin21* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != CHECK
  fail me;
#endif
  return 0;
}
