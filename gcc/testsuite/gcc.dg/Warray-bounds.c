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

    a[-1] = 0;             /* { dg-warning "6:array subscript" } */
    a[ 0] = 0;
    a[ 1] = 0;


    a[ 9] = 0;
    a[10] = 0;             /* { dg-warning "6:array subscript" } */
    a[11] = 0;             /* { dg-warning "6:array subscript" } */
    a[2 * n() - 11] = 1;    /* { dg-warning "6:array subscript" } */
    a[2 * n() - 10] = 1;
    a[2 * n() -  1] = 1;
    a[2 * n() -  0] = 1;    /* { dg-warning "6:array subscript" } */

    b[-1] = 0;             /* { dg-warning "6:array subscript" } */
    b[ 0] = 0;
    b[ 1] = 0;
    b[ 9] = 0;
    b[10] = 0;             /* { dg-warning "6:array subscript" } */
    b[11] = 0;             /* { dg-warning "6:array subscript" } */
    b[2 * n() - 11] = 1;    /* { dg-warning "6:array subscript" } */
    b[2 * n() - 10] = 1;
    b[2 * n() -  1] = 1;
    b[2 * n() -  0] = 1;    /* { dg-warning "array subscript" } */

    c.c[-1] = 0;           /* { dg-warning "8:array subscript" } */
    c.c[ 0] = 0;
    c.c[ 1] = 0;
    c.c[ 9] = 0;
    c.c[10] = 0;           /* { dg-warning "8:array subscript" } */
    c.c[11] = 0;           /* { dg-warning "8:array subscript" } */
    c.c[2 * n() - 11] = 1;  /* { dg-warning "8:array subscript" } */
    c.c[2 * n() - 10] = 1;
    c.c[2 * n() -  1] = 1;
    c.c[2 * n() -  0] = 1;  /* { dg-warning "8:array subscript" } */

    g(&a[8]);
    g(&a[9]);
    g(&a[10]);
    g(&a[11]);             /* { dg-warning "array subscript" } */
    g(&a[-30]+10);             /* { dg-warning "array subscript" } */
    g(&a[-30]+30);

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

    if (10 < 10)
       a[10] = 0;
    if (10 < 10)
       b[10] = 0;
    if (-1 >= 0)
       c.c[-1] = 0;

    for (i = 20; i < 30; ++i)
             a[i] = 1;       /* { dg-warning "15:array subscript" } */

    return a;
}

