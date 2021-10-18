namespace N
{
  int x;
  extern int z;
}

struct S
{
  static int s;
};

#if __cpp_variable_templates >= 201304
template <int N>
int t = N;
#endif

void
foo (void)
{
  int y = 0, i;
  static int w;
  #pragma omp task default(firstprivate)	/* { dg-message "note: enclosing 'task'" } */
  {
    y++;	/* { dg-bogus "'y' not specified in enclosing 'task'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'task'" } */
    S::s++;	/* { dg-bogus "'S::s' not specified in enclosing 'task'" } */
    N::x++;	/* { dg-error "'x' not specified in enclosing 'task'" } */
    N::z++;	/* { dg-error "'z' not specified in enclosing 'task'" } */
#if __cpp_variable_templates >= 201304
    t<5>++;	/* { dg-error "'t' not specified in enclosing 'task'" "" { target c++14 } } */
#endif
  }
  #pragma omp taskloop default(firstprivate)	/* { dg-message "note: enclosing 'taskloop'" } */
  for (i = 0; i < 64; i++)
    {
      y++;	/* { dg-bogus "'y' not specified in enclosing 'taskloop'" } */
      w++;	/* { dg-bogus "'w' not specified in enclosing 'taskloop'" } */
      S::s++;	/* { dg-bogus "'S::s' not specified in enclosing 'taskloop'" } */
      N::x++;	/* { dg-error "'x' not specified in enclosing 'taskloop'" } */
      N::z++;	/* { dg-error "'z' not specified in enclosing 'taskloop'" } */
#if __cpp_variable_templates >= 201304
      t<5>++;	/* { dg-error "'t' not specified in enclosing 'taskloop'" "" { target c++14 } } */
#endif
    }
  #pragma omp teams default(firstprivate)	/* { dg-message "note: enclosing 'teams'" } */
  {
    y++;	/* { dg-bogus "'y' not specified in enclosing 'teams'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'teams'" } */
    S::s++;	/* { dg-bogus "'S::s' not specified in enclosing 'teams'" } */
    N::x++;	/* { dg-error "'x' not specified in enclosing 'teams'" } */
    N::z++;	/* { dg-error "'z' not specified in enclosing 'teams'" } */
#if __cpp_variable_templates >= 201304
    t<5>++;	/* { dg-error "'t' not specified in enclosing 'teams'" "" { target c++14 } } */
#endif
  }
  #pragma omp parallel default(firstprivate)	/* { dg-message "note: enclosing 'parallel'" } */
  {
    y++;	/* { dg-bogus "'y' not specified in enclosing 'parallel'" } */
    w++;	/* { dg-bogus "'w' not specified in enclosing 'parallel'" } */
    S::s++;	/* { dg-bogus "'S::s' not specified in enclosing 'parallel'" } */
    N::x++;	/* { dg-error "'x' not specified in enclosing 'parallel'" } */
    N::z++;	/* { dg-error "'z' not specified in enclosing 'parallel'" } */
#if __cpp_variable_templates >= 201304
    t<5>++;	/* { dg-error "'t' not specified in enclosing 'parallel'" "" { target c++14 } } */
#endif
  }
  #pragma omp task default(private)	/* { dg-message "note: enclosing 'task'" } */
  {
    y = 1;	/* { dg-bogus "'y' not specified in enclosing 'task'" } */
    w = 1;	/* { dg-bogus "'w' not specified in enclosing 'task'" } */
    S::s = 1;	/* { dg-bogus "'S::s' not specified in enclosing 'task'" } */
    N::x++;	/* { dg-error "'x' not specified in enclosing 'task'" } */
    N::z++;	/* { dg-error "'z' not specified in enclosing 'task'" } */
#if __cpp_variable_templates >= 201304
    t<5>++;	/* { dg-error "'t' not specified in enclosing 'task'" "" { target c++14 } } */
#endif
  }
  #pragma omp taskloop default(private)	/* { dg-message "note: enclosing 'taskloop'" } */
  for (i = 0; i < 64; i++)
    {
      y = 1;	/* { dg-bogus "'y' not specified in enclosing 'taskloop'" } */
      w = 1;	/* { dg-bogus "'w' not specified in enclosing 'taskloop'" } */
      S::s = 1;	/* { dg-bogus "'S::s' not specified in enclosing 'taskloop'" } */
      N::x++;	/* { dg-error "'x' not specified in enclosing 'taskloop'" } */
      N::z++;	/* { dg-error "'z' not specified in enclosing 'taskloop'" } */
#if __cpp_variable_templates >= 201304
      t<5>++;	/* { dg-error "'t' not specified in enclosing 'taskloop'" "" { target c++14 } } */
#endif
    }
  #pragma omp teams default(private)	/* { dg-message "note: enclosing 'teams'" } */
  {
    y = 1;	/* { dg-bogus "'y' not specified in enclosing 'teams'" } */
    w = 1;	/* { dg-bogus "'w' not specified in enclosing 'teams'" } */
    S::s = 1;	/* { dg-bogus "'S::s' not specified in enclosing 'teams'" } */
    N::x++;	/* { dg-error "'x' not specified in enclosing 'teams'" } */
    N::z++;	/* { dg-error "'z' not specified in enclosing 'teams'" } */
#if __cpp_variable_templates >= 201304
    t<5>++;	/* { dg-error "'t' not specified in enclosing 'teams'" "" { target c++14 } } */
#endif
  }
  #pragma omp parallel default(private)	/* { dg-message "note: enclosing 'parallel'" } */
  {
    y = 1;	/* { dg-bogus "'y' not specified in enclosing 'parallel'" } */
    w = 1;	/* { dg-bogus "'w' not specified in enclosing 'parallel'" } */
    S::s = 1;	/* { dg-bogus "'S::s' not specified in enclosing 'parallel'" } */
    N::x++;	/* { dg-error "'x' not specified in enclosing 'parallel'" } */
    N::z++;	/* { dg-error "'z' not specified in enclosing 'parallel'" } */
#if __cpp_variable_templates >= 201304
    t<5>++;	/* { dg-error "'t' not specified in enclosing 'parallel'" "" { target c++14 } } */
#endif
  }
}
