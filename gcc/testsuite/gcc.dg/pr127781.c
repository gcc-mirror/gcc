/* { dg-do compile } */
/* { dg-options "-O3" } */
typedef struct { long size; const short *data; } SV;
long findString(SV h, long from, SV n);

static inline int checkSV(const SV *p)
{
    long k = 0;
    for (long i = 0; i < p->size; ++i)
        k += p->data[i] != 0;
    return k == p->size && p->size < 100;
}

static inline long indexOf(const SV *h, SV n, long from)
{
    if (!checkSV(&n))
        __builtin_unreachable();
    if (!(from >= 0 && from <= h->size + 2))
        __builtin_unreachable();
    return findString(*h, from, n);
}

int scanForToken(const SV *h, SV sought)
{
    if (!checkSV(&sought))
        __builtin_unreachable();
    long n = sought.size;
    if (!(n > 0))
        __builtin_unreachable();
    long idx = -n;
    int count = 0;
    while ((idx = indexOf(h, sought, idx + n)) >= 0)
        ++count;
    return count;
}

int scanForSigns(const SV *h)
{
    static const short minus[] = { 0x2212 };
    static const short pair[] = { '-', '4' };
    static const short hyph[] = { '-' };
    const SV list[] = { { 1, minus }, { 2, pair }, { 1, hyph } };
    int r = 0;
    for (int i = 0; i < 3; ++i)
        r = r * 10 + scanForToken(h, list[i]);
    return r;
}
