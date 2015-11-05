int bar (int);
int baz (int *);

template <int N>
void
f1 (int x)
{
  int i = 0, j = 0;
  #pragma omp for
  for (i = 0; i < 16; i++)
    ;
  #pragma omp for
  for (i = 0; 16 > i; i++)
    ;
  #pragma omp for
  for (i = 0; i < 16; i = i + 2)
    ;
  #pragma omp for
  for (i = 0; i < 16; i = 2 + i)
    ;
  #pragma omp for
  for (i = i; i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 2 * (i & x); i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = bar (i); i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = baz (&i); i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 2 * i + 17; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; 2 * i + 17 > i; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; bar (i) > i; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i <= baz (&i); i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i <= i; i++) /* { dg-error "invalid controlling predicate|condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i += i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i = i + 2 * i) /* { dg-error "invalid increment expression|increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i = i + i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i = i + bar (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i = baz (&i) + i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i += bar (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (i = 5; i < 16; i += baz (&i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = 0; j < 16; j += 2)
      ;
  #pragma omp for collapse(2)
  for (i = j; i < 16; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    for (j = i; j < 16; j += 2)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = i + 2)
    for (j = i + 3; j < 16; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++)
    for (j = baz (&i); j < 16; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 16; j > (i & x); j--)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 0; j < i; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 0; j < i + 4; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < j + 4; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < j; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < bar (j); i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (j = 0; j < baz (&i); j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i += j) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = 0; j < 16; j += i)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = j + i) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = 0; j < 16; j = j + i)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i = bar (j) + i) /* { dg-error "increment expression refers to iteration variable" } */
    for (j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j = j + baz (&i)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
}

template <int N>
void
f2 (int x)
{
  #pragma omp for
  for (int i = 0; i < 16; i++)
    ;
  #pragma omp for
  for (int i = 0; 16 > i; i++)
    ;
  #pragma omp for
  for (int i = 0; i < 16; i = i + 2)
    ;
  #pragma omp for
  for (int i = 0; i < 16; i = 2 + i)
    ;
  #pragma omp for
  for (int i = i; i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 2 * (i & x); i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = bar (i); i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = baz (&i); i < 16; i++) /* { dg-error "initializer expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 2 * i + 17; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; 2 * i + 17 > i; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; bar (i) > i; i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i <= baz (&i); i++) /* { dg-error "condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i <= i; i++) /* { dg-error "invalid controlling predicate|condition expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i += i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i = i + 2 * i) /* { dg-error "invalid increment expression|increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i = i + i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i = i + bar (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i = baz (&i) + i) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i += bar (i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for
  for (int i = 5; i < 16; i += baz (&i)) /* { dg-error "increment expression refers to iteration variable" } */
    ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i = i + 2)
    for (int j = 0; j < 16; j += 2)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i = i + 2) /* { dg-error "initializer expression refers to iteration variable" } */
    for (int j = i; j < 16; j += 2)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i = i + 2)
    for (int j = i + 3; j < 16; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++)
    for (int j = baz (&i); j < 16; j += 2) /* { dg-error "initializer expression refers to iteration variable" } */
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (int j = 16; j > (i & x); j--)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (int j = 0; j < i; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (int j = 0; j < i + 4; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++) /* { dg-error "condition expression refers to iteration variable" } */
    for (int j = 0; j < baz (&i); j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++) /* { dg-error "increment expression refers to iteration variable" } */
    for (int j = 0; j < 16; j += i)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++) /* { dg-error "increment expression refers to iteration variable" } */
    for (int j = 0; j < 16; j = j + i)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i++)
    for (int j = 0; j < 16; j = j + baz (&i)) /* { dg-error "increment expression refers to iteration variable" } */
      ;
}

template <int N>
void
f3 ()
{
  int j = 0;
  #pragma omp for collapse(2)
  for (int i = j; i < 16; i = i + 2)
    for (int j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < j + 4; i++)
    for (int j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < j; i++)
    for (int j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < bar (j); i++)
    for (int j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i += j)
    for (int j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i = j + i)
    for (int j = 0; j < 16; j++)
      ;
  #pragma omp for collapse(2)
  for (int i = 0; i < 16; i = bar (j) + i)
    for (int j = 0; j < 16; j++)
      ;
}

void
foo ()
{
  f1 <0> (0);
  f2 <0> (0);
  f3 <0> ();
}
