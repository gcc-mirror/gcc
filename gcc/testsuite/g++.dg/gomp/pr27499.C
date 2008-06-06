// PR c/27499
// { dg-do compile }

extern void bar (unsigned int);

void
foo (void)
{
  unsigned int i;
#pragma omp for
  for (i = 0; i < 64; ++i)
    bar (i);
}
