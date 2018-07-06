/* { dg-do compile } */
/* { dg-options "-O3" } */
static inline __attribute__ ((__always_inline__)) int fn1 () { return 0; }
static __attribute__ ((target ("inline-all-stringops"))) int fn2 () { fn1 (); }

int main()
{
  fn2();
}

