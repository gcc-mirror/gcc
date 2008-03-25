/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

inline int foo (int i)
{
    if (i) return 1; /* { dg-warning "is used uninitialized" } */
    return 0;
}

void baz();

void bar()
{
    int j;           /* { dg-message "was declared here" } */
    for (; foo(j); ++j)
        baz();
}
