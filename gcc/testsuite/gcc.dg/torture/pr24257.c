/* { dg-do compile } */
/* { dg-options "-O -fgcse -fgcse-sm" } */

typedef struct A {
    int buf, left;
} A;

static void flush(A *s, int n)
{
    s->buf <<= n;

    while (s->left < 32) {
        s->buf <<= 8;
        s->left += 8;
    }

    s->buf=0;
}

void oof(A *s, int n)
{
    s->buf = n;
    s->left = n;

    flush(s, n);
}
