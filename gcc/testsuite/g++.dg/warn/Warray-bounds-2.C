/* { dg-do compile } */
/* Test that -Warray-bounds is enabled by -Wall */
/* { dg-options "-O2 -Wall" } */

int a[10];

int* f(void) {

    a[-1] = 0;             /* { dg-warning "array subscript" } */

    return a;
}

