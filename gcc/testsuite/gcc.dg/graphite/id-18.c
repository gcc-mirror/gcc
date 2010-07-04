long do_hash (const char * lo, const char * hi)
{
	int val = 0;
	for (; lo < hi; ++lo)
		val = *lo;
	return val;
}
