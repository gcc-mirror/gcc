/* PR127693 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre3" } */

typedef struct { long size; const short *data; } SV;
typedef struct { SV given; } Scanner;
long findString(SV h, long from, SV n);
const short *strchr16(const short *b, const short *e, short c);

static inline long findChar(SV h, long from, short c)
{
    if ((unsigned long)from < (unsigned long)h.size) {
        const short *n = strchr16(h.data + from, h.data + h.size, c);
        if (n != h.data + h.size)
            return n - h.data;
    }
    return -1;
}

static inline long indexOf(const SV *h, SV n, long from)
{
    if (__builtin_constant_p(n.size) && n.size == 1)
        return findChar(*h, from, n.data[0]);
    return findString(*h, from, n);
}

static int scanForToken(Scanner *s, SV sought)
{
    long n = sought.size;
    long idx = -n;
    int count = 0;
    while ((idx = indexOf(&s->given, sought, idx + n)) >= 0)
        ++count;
    return count;
}

__attribute__((noinline)) int scanForSigns(Scanner *s)
{
    static const short minus[] = { 0x2212 };
    SV m = { 1, minus };
    return scanForToken(s, m);
}

int f(SV h)
{
    Scanner s = { h };
    return scanForSigns(&s);
}

/* { dg-final { scan-tree-dump-not "unreachable" "fre3" } } */
