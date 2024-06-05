/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

/* Test that omp::sequence is handled properly in a loop nest, but that
   invalid attribute specifiers are rejected.  */

extern void dummy (int);

void
test2 (void)
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (masked)]]  /* { dg-error "expected 'for' loop or OpenMP loop transformation construct" } */
    for (int j = 0; j != 100; ++j) /* { dg-error "loop not permitted" } */
      dummy (i);
}

void
test3 (void)
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (unroll, partial)]]  /* { dg-error "attributes on the same statement" } */
    [[omp::directive (masked)]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test4 (void)
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::sequence (directive (unroll, partial),
		     directive (masked))]]  /* { dg-error "loop nest expected" } */
    for (int j = 0; j != 100; ++j)
      dummy (i); /* { dg-error "declared" } */
}

void
test5 (void)
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::sequence (directive (masked),  /* { dg-error "expected 'for' loop or OpenMP loop transformation construct" } */
		     directive (unroll, partial))]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test6 (void)
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (unroll, partial),  /* { dg-error "attributes on the same statement" } */
      omp::directive (masked)]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}
