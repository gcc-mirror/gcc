/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

typedef unsigned int DWORD;

static void g(DWORD * p, int n)
{
        int i;

        for (i = 0; i < n && !p[n - 1]; i++);  /* { dg-bogus "subscript is above array bounds" } */
}

void f() {
        DWORD arr[8];

        g(arr, 4);
}
