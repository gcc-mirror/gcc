#ifdef L_fixunsdfdi
#define EXPD(fp)	(((fp.l.upper) >> 20) & 0x7FF)
#define EXCESSD		1022
#define SIGNBIT		0x80000000
#define SIGND(fp)	((fp.l.upper) & SIGNBIT)
#define MANTD_LL(fp)	((fp.ll & (HIDDEND_LL-1)) | HIDDEND_LL)
#define FRACD_LL(fp)	(fp.ll & (HIDDEND_LL-1))
#define HIDDEND_LL	((long long)1 << 52)

union double_long {
    double d;
    struct {
      long upper;
      unsigned long lower;
    } l;
    long long ll;
};


/* convert double to unsigned int */
unsigned long long
__fixunsdfdi (double a1)
{
    register union double_long dl1;
    register int exp;
    register long long l;

    dl1.d = a1;

    /* +/- 0, denormalized, negativ */

    if (!EXPD (dl1) || SIGND(dl1))
      return 0;

    exp = EXPD (dl1) - EXCESSD - 53;

    /* number < 1 */

    if (exp < -53)
      return 0;

    /* NaN */

    if ((EXPD(dl1) == 0x7ff) && (FRACD_LL(dl1) != 0)) /* NaN */
      return 0x0ULL;

    /* Number big number & + inf */

    if (exp >= 12) {
      return 0xFFFFFFFFFFFFFFFFULL;
    }

    l = MANTD_LL(dl1);

    /* shift down until exp < 12 or l = 0 */
    if (exp > 0)
      l <<= exp;
    else 
      l >>= -exp;

    return l;
}
#define __fixunsdfdi ___fixunsdfdi
#endif
#undef L_fixunsdfdi

#ifdef L_fixdfdi
#define EXPD(fp)	(((fp.l.upper) >> 20) & 0x7FF)
#define EXCESSD		1022
#define SIGNBIT		0x80000000
#define SIGND(fp)	((fp.l.upper) & SIGNBIT)
#define MANTD_LL(fp)	((fp.ll & (HIDDEND_LL-1)) | HIDDEND_LL)
#define FRACD_LL(fp)	(fp.ll & (HIDDEND_LL-1))
#define HIDDEND_LL	((long long)1 << 52)

union double_long {
    double d;
    struct {
      long upper;
      unsigned long lower;
    } l;
    long long ll;
};

/* convert double to int */
long long
__fixdfdi (double a1)
{
    register union double_long dl1;
    register int exp;
    register long long l;

    dl1.d = a1;

    /* +/- 0, denormalized */

    if (!EXPD (dl1))
      return 0;

    exp = EXPD (dl1) - EXCESSD - 53;

    /* number < 1 */

    if (exp < -53)
      return 0;

    /* NaN */

    if ((EXPD(dl1) == 0x7ff) && (FRACD_LL(dl1) != 0)) /* NaN */
      return 0x8000000000000000ULL;

    /* Number big number & +/- inf */

    if (exp >= 11) {
	l = (long long)1<<63;
	if (!SIGND(dl1))
	    l--;
	return l;
    }

    l = MANTD_LL(dl1);

    /* shift down until exp < 12 or l = 0 */
    if (exp > 0)
      l <<= exp;
    else 
      l >>= -exp;

    return (SIGND (dl1) ? -l : l);
}
#define __fixdfdi ___fixdfdi
#endif
#undef L_fixdfdi

