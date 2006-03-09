// { dg-do compile }

extern void bar (void);

int main (void)
{
  int i;
#pragma omp parallel for nowait /* { dg-error "'nowait'" } */
  for (i = 0; i < 10; i++)
    bar ();
}
