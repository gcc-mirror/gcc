/* Verify the padding initialization for pattern initialization, we always emit
 * a call to __builtin_clear_padding to initialize the paddings to zero.  */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-tree-gimple" } */


struct test_small_hole {
  int one;
  char two;
  /* 3 byte padding hole here. */
  int three;
  unsigned long long four __attribute__((aligned (8)));
};

extern void g (struct test_small_hole);
void foo(int a)
{
  struct test_small_hole s; 
  g(s);
}

/* { dg-final { scan-tree-dump ".DEFERRED_INIT \\(24, 1, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_clear_padding" "gimple" } } */
