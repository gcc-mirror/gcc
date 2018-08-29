void
foo (void)
{
#pragma acc parallel loop tile (2, 3)
  for (short i = 0; i < 10; ++i)
    for (short j = 0; j < 10; ++j)
      ;
}
