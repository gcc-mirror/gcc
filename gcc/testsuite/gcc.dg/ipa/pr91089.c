/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fdump-ipa-fnsummary-details --param ipa-max-switch-predicate-bounds=10 -fno-inline" } */

int fn ();

int data;

int callee (int i)
{
  switch (i % 128)
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

int fn2 ();

int callee_complex_predicate (int i)
{
  switch (i)
    {
      case 0:
	fn ();
	fn ();
	fn ();
      case 1:
	fn ();
	fn ();
      case -1:
	fn ();
      case -2:
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
	data += i;
	break;
    }

  if (i == 1000)
    {
      int j;

      for (j = 0; j < 100; j++)
	fn2 ();
    }
  return i + 3;
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
/* { dg-final { scan-ipa-dump-not "op0,\\(# % 128\\) < -127" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0,\\(# % 128\\) > -126" "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0,\\(# % 128\\) != -8"  "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0,\\(# % 128\\) != 0"   "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0,\\(# % 128\\) < 5"    "fnsummary" } } */
/* { dg-final { scan-ipa-dump "op0,\\(# % 128\\) > 7"    "fnsummary" } } */
/* { dg-final { scan-ipa-dump "loop depth: 1 .+ time:\[ \]*\[0-9\]+ predicate: \\(op0 == 1000\\)\[\r\n]+" "fnsummary" } } */
