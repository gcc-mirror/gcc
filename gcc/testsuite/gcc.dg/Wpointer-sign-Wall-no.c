/* This is from PR c/25892.  See Wpointer-sign.c for more details.  */

/* { dg-options "-Wno-pointer-sign -Wall" } */

void foo(unsigned long* ulp);

void bar(long* lp) {
  foo(lp);
}
