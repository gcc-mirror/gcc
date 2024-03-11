/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand -mno-sse" } */

struct a { long arr[30]; };

__seg_gs struct a m;
void bar (struct a *dst) { *dst = m; }

/* { dg-final { scan-rtl-dump-not "libcall" "expand" } } */
