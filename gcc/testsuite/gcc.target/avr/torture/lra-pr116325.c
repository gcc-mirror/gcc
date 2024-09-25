typedef __SIZE_TYPE__ size_t;
typedef __UINT8_TYPE__ uint8_t;

void swapfunc (char *a, char *b, int n)
{
    do
    {
        char t = *a;
        *a++ = *b;
        *b++ = t;
    } while (--n > 0);
}


typedef int cmp_t (const void*, const void*);

#define min(a, b)       ((a) < (b) ? (a) : (b))

#define swap(a, b) \
    swapfunc (a, b, es)

#define vecswap(a, b, n) \
    if ((n) > 0) swapfunc (a, b, n)

static char*
med3 (char *a, char *b, char *c, cmp_t *cmp)
{
    return cmp (a, b) < 0
        ? (cmp (b, c) < 0 ? b : (cmp (a, c) < 0 ? c : a ))
        : (cmp (b, c) > 0 ? b : (cmp (a, c) < 0 ? a : c ));
}

void
qsort (void *a, size_t n, size_t es, cmp_t *cmp)
{
    char *pa, *pb, *pc, *pd, *pl, *pm, *pn;
    int d, r, swap_cnt;

loop:
    swap_cnt = 0;
    if (n < 7)
    {
        for (pm = (char*) a + es; pm < (char*) a + n * es; pm += es)
            for (pl = pm; pl > (char*) a && cmp (pl - es, pl) > 0; pl -= es)
                swap (pl, pl - es);
        return;
    }
    pm = (char*) a + (n / 2) * es;
    if (n > 7)
    {
        pl = a;
        pn = (char*) a + (n - 1) * es;
        if (n > 40)
        {
            d = (n / 8) * es;
            pl = med3 (pl, pl + d, pl + 2 * d, cmp);
            pm = med3 (pm - d, pm, pm + d, cmp);
            pn = med3 (pn - 2 * d, pn - d, pn, cmp);
        }
        pm = med3 (pl, pm, pn, cmp);
    }
    swap (a, pm);
    pa = pb = (char*) a + es;

    pc = pd = (char*) a + (n - 1) * es;
    for (;;)
    {
        while (pb <= pc && (r = cmp (pb, a)) <= 0)
        {
            if (r == 0)
            {
                swap_cnt = 1;
                swap (pa, pb);
                pa += es;
            }
            pb += es;
        }
        while (pb <= pc && (r = cmp (pc, a)) >= 0)
        {
            if (r == 0)
            {
                swap_cnt = 1;
                swap (pc, pd);
                pd -= es;
            }
            pc -= es;
        }
        if (pb > pc)
            break;
        swap (pb, pc);
        swap_cnt = 1;
        pb += es;
        pc -= es;
    }
    if (swap_cnt == 0)
    {
        for (pm = (char*) a + es; pm < (char*) a + n * es; pm += es)
            for (pl = pm; pl > (char*) a && cmp (pl - es, pl) > 0; pl -= es)
                swap (pl, pl - es);
        return;
    }

    pn = (char*) a + n * es;
    r = min (pa - (char*) a, pb - pa);
    vecswap (a, pb - r, r);
    r = min (pd - pc, (int) (pn - pd - es));
    vecswap (pb, pn - r, r);
    if ((r = pb - pa) > (int) es)
        qsort(a, r / es, es, cmp);
    if ((r = pd - pc) > (int) es)
    {
        /* Iterate rather than recurse to save stack space */
        a = pn - r;
        n = r / es;
        goto loop;
    }
}
