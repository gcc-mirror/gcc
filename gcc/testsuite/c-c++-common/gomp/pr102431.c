/* PR middle-end/102431 */

struct S { int s; } s;
void add (struct S *, struct S *);
void init (struct S *);
void bar (int i, struct S *);
#pragma omp declare reduction (+:struct S:add (&omp_out, &omp_in)) initializer (init (&omp_priv))

void
foo (void)
{
  int i;
  #pragma omp loop bind(teams) reduction(+:s)
  for (i = 0; i < 8; i++)
    bar (i, &s);
}
