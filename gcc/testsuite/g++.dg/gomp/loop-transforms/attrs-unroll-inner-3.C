// { dg-do compile { target c++11 } }

// Test that omp::sequence is handled properly in a loop nest, but that
// invalid attribute specifiers are rejected.

extern void dummy (int);

void
test1 ()
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::sequence (directive (unroll, partial))]]  // OK
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test2 ()
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (masked)]]  // { dg-error "loop nest expected" }
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test3 ()
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (unroll, partial)]]  // { dg-error "attributes on the same statement" }
    [[omp::directive (masked)]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test4 ()
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::sequence (directive (unroll, partial),
		     directive (masked))]]  // { dg-error "loop nest expected" }
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test5 ()
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::sequence (directive (masked),  // { dg-error "loop nest expected" }
		     directive (unroll, partial))]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

void
test6 ()
{
  [[omp::directive (target parallel for collapse(2))]]
  for (int i = -300; i != 100; ++i)
    [[omp::directive (unroll, partial),  // { dg-error "attributes on the same statement" }
      omp::directive (masked)]]
    for (int j = 0; j != 100; ++j)
      dummy (i);
}

