/* Sparc w/128-bit long double bombed on this because even though
   the trunctfdf libcall passed the long double by reference, the
   libcall was still marked as LCT_CONST instead of LCT_PURE.  */

double *copy(long double *first, long double *last, double *result)
{
	int n;
	for (n = last - first; n > 0; --n) {
		*result = *first;
		++first;
		++result;
	}
	return result;
}
