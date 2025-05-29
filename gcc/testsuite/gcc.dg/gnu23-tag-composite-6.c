/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

int f()
{
    typedef struct foo bar;
    struct foo { typeof(({ (struct foo { bar * x; }){ }; })) * x; } *q;
    typeof(q->x) p;
    1 ? p : q;
}

