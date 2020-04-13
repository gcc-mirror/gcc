/* Basic test of the -mmacosx-version-min option.  */

/* Darwin4 corresponds to MacOS 10.0.  */
/* { dg-options "-mmacosx-version-min=10.1" { target *-*-darwin[456789]* } } */
/* Later Darwin linkers decline to link for less than Darwin8/MacOS 10.4.
   However, we need to make the link for 10.6 because the relevant libgcc_s
   shim files for 10.4 and 10.5 are also not installed in later SDKs.  */
/* { dg-options "-mmacosx-version-min=10.6" { target *-*-darwin1[01234567]* } } */
/* From XCode 11.4 on 10.14/15 10.6 and 10.7 are also deprecated.  */
/* { dg-options "-mmacosx-version-min=10.8" { target *-*-darwin1[89]* } } */
/* { dg-do link { target *-*-darwin* } } */

int main()
{
  return 0;
}
