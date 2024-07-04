/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

int main () {
	_Decimal64 d64 = -0.DD;

	if (d64 != 0.DD)
		__builtin_abort ();

	if (d64 != -0.DD)
		__builtin_abort ();

	return 0;
}
