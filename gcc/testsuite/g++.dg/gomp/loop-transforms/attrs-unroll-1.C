// { dg-do compile { target c++11 } }

extern void dummy (int);

void
test1 ()
{
[[omp::directive (unroll partial)]]
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test2 ()
{
[[omp::directive (unroll partial(10))]]
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test3 ()
{
[[omp::directive (unroll full)]]
  for (int i = 0; i < 100; ++i)
    dummy (i);
}

void
test4 ()
{
[[omp::directive (unroll full)]]
  for (int i = 0; i > 100; ++i)
    dummy (i);
}

void
test5 ()
{
[[omp::directive (unroll full)]]
  for (int i = 1; i <= 100; ++i)
    dummy (i);
}

void
test6 ()
{
[[omp::directive (unroll full)]]
  for (int i = 200; i >= 100; i--)
    dummy (i);
}

void
test7 ()
{
[[omp::directive (unroll full)]]
  for (int i = -100; i > 100; ++i)
    dummy (i);
}

void
test8 ()
{
[[omp::directive (unroll full)]]
  for (int i = 100; i > -200; --i)
    dummy (i);
}

void
test9 ()
{
[[omp::directive (unroll full)]]
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test10 ()
{
[[omp::directive (unroll full)]]
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test12 ()
{
[[omp::sequence (directive (unroll full),
  directive (unroll partial),
  directive (unroll partial))]]
  for (int i = -300; i != 100; ++i)
    dummy (i);
}

void
test13 ()
{
  for (int i = 0; i < 100; ++i)
[[omp::sequence (directive (unroll full),
  directive (unroll partial),
  directive (unroll partial))]]
  for (int j = -300; j != 100; ++j)
    dummy (i);
}

void
test14 ()
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
test15 ()
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
