/* { dg-do compile { target { c || c++11 } } } */
/* { dg-additional-options "-std=c23" { target c } } */

extern void dummy (int);

void
test1 (void)
{
  [[omp::directive (unroll partial)]]
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test2 (void)
{
  [[omp::directive (unroll partial(10))]]
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test3 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test4 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = 0; i > 100; ++i)
    dummy (i);
}

void
test5 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = 1; i <= 100; ++i)
    dummy (i);
}

void
test6 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = 200; i >= 100; i--)
    dummy (i);
}

void
test7 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = -100; i > 100; ++i)
    dummy (i);
}

void
test8 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = 100; i > -200; --i)
    dummy (i);
}

void
test9 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test10 (void)
{
  [[omp::directive (unroll full)]]
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test12 (void)
{
  [[omp::sequence (directive (unroll full),
		   directive (unroll partial),
		   directive (unroll partial))]]
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test13 (void)
{
  for (int i = 0; i < 100; ++i)
  [[omp::sequence (directive (unroll full),
		   directive (unroll partial),
		   directive (unroll partial))]]
  for (int j = -300; j != 100; ++j)
    dummy (i);
}

void
test14 (void)
{
  [[omp::directive (for)]]
  for (int i = 0; i < 100; ++i)
    [[omp::sequence (directive (unroll full),
		     directive (unroll partial),
		     directive (unroll partial))]]
  for (int j = -300; j != 100; ++j)
    dummy (i);
}

void
test15 (void)
{
  [[omp::directive (for)]]
  for (int i = 0; i < 100; ++i)
    {
      dummy (i);

      [[omp::sequence (directive (unroll full),
		       directive (unroll partial),
		       directive (unroll partial))]]
      for (int j = -300; j != 100; ++j)
	dummy (j);

      dummy (i);
    }
 }
