/* { dg-do compile } */
/* { dg-options "-O2 -fstrict-aliasing -fdump-tree-phiopt-details" } */
/* PR tree-optimization/125945 */
/* These factoring out of loads should happen only after the vectorizer for this code. */
typedef short s16;
struct S { s16 tag; s16 key; };
void f(s16 *dst, const struct S *a, const struct S *b, int n)
{
    for (int i = 0; i < n; i++) {
        s16 ka = a[i].key;
        s16 kb = b[i].key;
        if (ka >= kb)
            dst[i] = a[i].tag;
        else
            dst[i] = b[i].tag;
    }
}
void f1(s16 *dst, const struct S *a, const struct S *b, int n)
{
    for (int i = 0; i < n; i++) {
        s16 ka = a[i].key;
        s16 kb = b[i].key;
        s16 t;
        if (ka >= kb)
            t = a[i].tag;
        else
            t = b[i].tag;
        dst[i] = t;
    }
}
void f2(s16 *dst, const struct S *a, const struct S *b, int n)
{
    for (int i = 0; i < n; i++) {
        s16 ka = a[i].key;
        s16 kb = b[i].key;
        s16 t;
        const struct S *tt = &a[i];
        const struct S *tt1 = &b[i];
        if (ka >= kb)
            t = tt->tag;
        else
            t = tt1->tag;
        dst[i] = t;
    }
}
void f3(s16 *dst, const struct S *a, const struct S *b, int n)
{
    for (int i = 0; i < n; i++) {
        s16 ka = a[i].key;
        s16 kb = b[i].key;
        s16 t;
        const s16 *tt = &a[i].tag;
        const s16 *tt1 = &b[i].tag;
        if (ka >= kb)
            t = *tt;
        else
            t = *tt1;
        dst[i] = t;
    }
}

/* { dg-final { scan-tree-dump-not "changed to factor out load from" "phiopt3" } } */
/* { dg-final { scan-tree-dump-not "changed to factor out load from" "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "changed to factor out load from" 4 "phiopt4" } } */
