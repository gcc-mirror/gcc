/* { dg-do compile } */
/* Force power7 to avoid possible error message on AltiVec ABI change.  */
/* { dg-options "-mdejagnu-cpu=power7" } */

/* Verify there is an error message for incompatible -maltivec and -mvsx
   even when they are specified by target attributes.  */

int __attribute__ ((target ("no-altivec,vsx")))
test1 (void)
{
  /* { dg-error "'-mvsx' and '-mno-altivec' are incompatible" "" { target *-*-* } .-1 } */
  return 0;
}

int __attribute__ ((target ("vsx,no-altivec")))
test2 (void)
{
  /* { dg-error "'-mvsx' and '-mno-altivec' are incompatible" "" { target *-*-* } .-1 } */
  return 0;
}
