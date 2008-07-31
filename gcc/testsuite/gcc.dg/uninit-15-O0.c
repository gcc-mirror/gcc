/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

/* XFAIL for now, the uninitialized pass runs before inlining only at -O0.  */

inline int __attribute__((always_inline))
foo (int i)
{
    if (i) return 1; /* { dg-warning "is used uninitialized" {} { xfail *-*-* } } */
    return 0;
}

void baz();

void bar()
{
    int j;           /* { dg-message "was declared here" {} { xfail *-*-* } } */
    for (; foo(j); ++j)
        baz();
}
