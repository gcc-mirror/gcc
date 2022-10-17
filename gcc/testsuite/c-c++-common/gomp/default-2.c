int x;
extern int z;

void
foo (void)
{
  int y = 0, i;
  static int w;
  #pragma omp task default(firstprivate)	/* { dg-message "note: enclosing 'task'" } */
  {
    y++;	/* { dg-bogus "'y' not specified in enclosing 'task'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'task'" } */
    x++;	/* { dg-error "'x' not specified in enclosing 'task'" } */
    z++;	/* { dg-error "'z' not specified in enclosing 'task'" } */
  }
  #pragma omp taskloop default(firstprivate)	/* { dg-message "note: enclosing 'taskloop'" } */
  for (i = 0; i < 64; i++)
    {
      y++;	/* { dg-bogus "'y' not specified in enclosing 'taskloop'" } */
      w++;	/* { dg-bogus "'w' not specified in enclosing 'taskloop'" } */
      x++;	/* { dg-error "'x' not specified in enclosing 'taskloop'" } */
      z++;	/* { dg-error "'z' not specified in enclosing 'taskloop'" } */
    }
  #pragma omp teams default(firstprivate)	/* { dg-message "note: enclosing 'teams'" } */
  {
    y++;	/* { dg-bogus "'y' not specified in enclosing 'teams'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'teams'" } */
    x++;	/* { dg-error "'x' not specified in enclosing 'teams'" } */
    z++;	/* { dg-error "'z' not specified in enclosing 'teams'" } */
  }
  #pragma omp parallel default(firstprivate)	/* { dg-message "note: enclosing 'parallel'" } */
  {
    y++;	/* { dg-bogus "'y' not specified in enclosing 'parallel'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'parallel'" } */
    x++;	/* { dg-error "'x' not specified in enclosing 'parallel'" } */
    z++;	/* { dg-error "'z' not specified in enclosing 'parallel'" } */
  }
  #pragma omp task default(private)	/* { dg-message "note: enclosing 'task'" } */
  {
    y = 1;	/* { dg-bogus "'y' not specified in enclosing 'task'" } */
    w = 1;	/* { dg-bogus "'w' not specified in enclosing 'task'" } */
    x++;	/* { dg-error "'x' not specified in enclosing 'task'" } */
    z++;	/* { dg-error "'z' not specified in enclosing 'task'" } */
  }
  #pragma omp taskloop default(private)	/* { dg-message "note: enclosing 'taskloop'" } */
  for (i = 0; i < 64; i++)
    {
      y = 1;	/* { dg-bogus "'y' not specified in enclosing 'taskloop'" } */
      w = 1;	/* { dg-bogus "'w' not specified in enclosing 'taskloop'" } */
      x++;	/* { dg-error "'x' not specified in enclosing 'taskloop'" } */
      z++;	/* { dg-error "'z' not specified in enclosing 'taskloop'" } */
    }
  #pragma omp teams default(private)	/* { dg-message "note: enclosing 'teams'" } */
  {
    y = 1;	/* { dg-bogus "'y' not specified in enclosing 'teams'" } */
    w = 1;	/* { dg-bogus "'w' not specified in enclosing 'teams'" } */
    x++;	/* { dg-error "'x' not specified in enclosing 'teams'" } */
    z++;	/* { dg-error "'z' not specified in enclosing 'teams'" } */
  }
  #pragma omp parallel default(private)	/* { dg-message "note: enclosing 'parallel'" } */
  {
    y = 1;	/* { dg-bogus "'y' not specified in enclosing 'parallel'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'parallel'" } */
    x++;	/* { dg-error "'x' not specified in enclosing 'parallel'" } */
    z++;	/* { dg-error "'z' not specified in enclosing 'parallel'" } */
  }
}
