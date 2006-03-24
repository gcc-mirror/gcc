// PR middle-end/26611
// { dg-do compile }

void
foo ()
{
#pragma omp parallel
  try
    {
    }
  catch (...)
    {
      int q = 1;
    }
}
