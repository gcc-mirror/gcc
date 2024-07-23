/* { dg-do compile } */
/* Force power7 to avoid possible error message on AltiVec ABI change.  */
/* { dg-options "-mdejagnu-cpu=power7" } */

/* Verify there is an error message for -mvsx incompatible with
   -mavoid-indexed-addresses even when they are specified by
   target attributes.  */

int __attribute__ ((target ("avoid-indexed-addresses,vsx")))
test1 (void)
{
  /* { dg-error "'-mvsx' and '-mavoid-indexed-addresses' are incompatible" "" { target *-*-* } .-1 } */
  return 0;
}

int __attribute__ ((target ("vsx,avoid-indexed-addresses")))
test2 (void)
{
  /* { dg-error "'-mvsx' and '-mavoid-indexed-addresses' are incompatible" "" { target *-*-* } .-1 } */
  return 0;
}

