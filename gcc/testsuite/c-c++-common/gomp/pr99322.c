/* PR middle-end/99322 */
/* { dg-do compile } */

void foo (void);
void qux (void *);

void
bar (void)
{
  #pragma omp parallel
  for (;;)
    for (int i = 0; i < 8; ++i)
      foo ();
  { lab:; }
  qux (&&lab);
}

void
baz (void)
{
  qux (&&lab);
  #pragma omp parallel
  for (;;)
    ;
  lab:;
}
