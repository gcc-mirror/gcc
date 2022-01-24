// PR middle-end/102431

struct S { S (); ~S (); S (const S &); void add (const S &); int s; } s;
void bar (int, S &);
#pragma omp declare reduction (+:S:omp_out.add (omp_in))

void
foo ()
{
  #pragma omp loop bind(teams) reduction(+:s)
  for (int i = 0; i < 8; i++)
    bar (i, s);
}
