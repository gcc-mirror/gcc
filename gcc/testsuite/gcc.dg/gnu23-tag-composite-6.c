/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

#define NEST(...) typeof(({ (__VA_ARGS__){ }; }))

int f()
{
    typedef struct foo bar;
    struct foo { NEST(struct foo { bar *x; }) *x; } *q;
    typeof(q->x) p0;
    typeof(q->x) p1;
    1 ? p0 : q;
    1 ? p1 : q;
    1 ? p0 : p1;
}

int g()
{
    typedef struct fo2 bar;
    struct fo2 { NEST(struct fo2 { NEST(struct fo2 { bar *x; }) * x; }) *x; } *q;
    typeof(q->x) p0;
    typeof(q->x->x) p1;
    typeof(q->x->x->x) p2;
    1 ? p0 : q;
    1 ? p1 : q;
    1 ? p2 : q;
    1 ? p0 : p1;
    1 ? p2 : p1;
    1 ? p0 : p2;
}

