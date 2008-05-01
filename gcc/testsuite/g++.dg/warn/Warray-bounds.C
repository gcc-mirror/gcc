/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int a[10];

static inline int n(void) {
    __SIZE_TYPE__ strlen(const char *s);
    return strlen("12345");
}

void g(int *p);
void h(int p);

int* f(void) {
    int b[10];
    int i;
    struct {
       int c[10];
    } c;
    int p[0], q[1], r[2], s[3], t[4];

    a[-1] = 0;             /* { dg-warning "array subscript" } */
    a[ 0] = 0;
    a[ 1] = 0;


    a[ 9] = 0;
    a[10] = 0;             /* { dg-warning "array subscript" } */
    a[11] = 0;             /* { dg-warning "array subscript" } */
    a[2 * n() - 11] = 0;    /* { dg-warning "array subscript" } */
    a[2 * n() - 10] = 0;
    a[2 * n() -  1] = 0;
    a[2 * n() -  0] = 0;    /* { dg-warning "array subscript" } */

    b[-1] = 0;             /* { dg-warning "array subscript" } */
    b[ 0] = 0;
    b[ 1] = 0;
    b[ 9] = 0;
    b[10] = 0;             /* { dg-warning "array subscript" } */
    b[11] = 0;             /* { dg-warning "array subscript" } */
    b[2 * n() - 11] = 0;    /* { dg-warning "array subscript" } */
    b[2 * n() - 10] = 0;
    b[2 * n() -  1] = 0;
    b[2 * n() -  0] = 0;    /* { dg-warning "array subscript" } */

    c.c[-1] = 0;           /* { dg-warning "array subscript" } */
    c.c[ 0] = 0;
    c.c[ 1] = 0;
    c.c[ 9] = 0;
    c.c[10] = 0;           /* { dg-warning "array subscript" } */
    c.c[11] = 0;           /* { dg-warning "array subscript" } */
    c.c[2 * n() - 11] = 0;  /* { dg-warning "array subscript" } */
    c.c[2 * n() - 10] = 0;
    c.c[2 * n() -  1] = 0;
    c.c[2 * n() -  0] = 0;  /* { dg-warning "array subscript" } */

    g(&a[8]);
    g(&a[9]);
    g(&a[10]);
    g(&a[11]);             /* { dg-warning "array subscript" } */
    g(&a[-30]+10);         /* { dg-warning "array subscript" } */
    g(&a[-30]+30);         /* { dg-warning "array subscript" } */

    g(&b[10]);
    g(&c.c[10]);
    g(&b[11]);             /* { dg-warning "array subscript" } */
    g(&c.c[11]);           /* { dg-warning "array subscript" } */

    g(&a[0]);
    g(&b[0]);
    g(&c.c[0]);

    g(&a[-1]);             /* { dg-warning "array subscript" } */
    g(&b[-1]);             /* { dg-warning "array subscript" } */ 
    h(sizeof a[-1]);
    h(sizeof a[10]);
    h(sizeof b[-1]);
    h(sizeof b[10]);
    h(sizeof c.c[-1]);
    h(sizeof c.c[10]);

    p[-1] = 0;             /* { dg-warning "array subscript" } */
    p[0] = 0;
    p[1] = 0;

    q[-1] = 0;             /* { dg-warning "array subscript" } */
    q[0] = 0;
    q[1] = 0;
    q[2] = 0;

    r[-1] = 0;             /* { dg-warning "array subscript" } */
    r[0] = 0;
    r[1] = 0;
    r[2] = 0;
    r[3] = 0;              /* { dg-warning "array subscript" } */

    s[-1] = 0;             /* { dg-warning "array subscript" } */
    s[0] = 0;
    s[1] = 0;
    s[2] = 0;
    s[3] = 0;
    s[4] = 0;              /* { dg-warning "array subscript" } */

    t[-1] = 0;             /* { dg-warning "array subscript" } */
    t[0] = 0;
    t[1] = 0;
    t[2] = 0;
    t[3] = 0;
    t[4] = 0;
    t[5] = 0;              /* { dg-warning "array subscript" } */

    if (10 < 10)
       a[10] = 0;
    if (10 < 10)
       b[10] = 0;
    if (-1 >= 0)
       c.c[-1] = 0;        /* { dg-warning "array subscript" } */

    for (i = 20; i < 30; ++i)
             a[i] = 1;       /* { dg-warning "array subscript" } */

    return a;
}
