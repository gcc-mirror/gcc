// PR middle-end/102492
// { dg-do compile }

struct S { S (int); };
void bar (S &);

void
foo ()
{
  #pragma omp simd
  for (int i = 0; i < 64; i++)
    {
      S s = 26;
      bar (s);
    }
}
