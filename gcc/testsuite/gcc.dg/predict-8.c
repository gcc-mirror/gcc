/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fdump-rtl-expand-details-blocks" } */

int foo(float a, float b) {
  if (a == b)
    return 1;
  else
    return 2;
}

/* { dg-final { scan-rtl-dump-times "99.\[678]. .guessed" 2 "expand"} } */
