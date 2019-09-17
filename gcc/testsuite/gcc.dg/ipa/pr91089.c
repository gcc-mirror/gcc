/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fdump-ipa-fnsummary-details --param ipa-max-switch-predicate-bounds=10 -fno-inline" } */

int fn ();

int data;

int callee (int i)
{
  switch (i)
    {
      case -126:  return i + 13;
      case -127:  return i + 5;
      case -8:    return i * i;
      case 0:     return i % 9;
      case 5:
      case 7:
      case 6:     return 3;
      default:
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
	fn ();
     }

  return data += i;
}

int caller ()
{
  return callee (-127) +
	 callee (-126) +
	 callee (-8) +
	 callee (0) +
	 callee (5) +
	 callee (6) +
	 callee (7) +
	 callee (100);
}
 
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of callee" 7 "cp" } } */
/* { dg-final { scan-ipa-dump "op0 < -127" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0 > -126" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0 != -8"  "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0 != 0"   "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0 < 5"    "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0 > 7"    "fnsummary" } } */
