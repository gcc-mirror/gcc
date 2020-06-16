void
foo ()
{
  int i = 0;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++)	// { dg-error "the same loop iteration variables 'i' used in multiple associated loops" }
    for (i = 1; i < 32; i++)
      ;
  #pragma omp taskloop collapse(2)
  for (int j = 0; j < 16; j++)	// { dg-error "the same loop iteration variables 'j' used in multiple associated loops" }
    for (j = 0; j < 16; j++)
      ;
}

template <int N>
void
bar ()
{
  int i = 0;
  #pragma omp for collapse(2)
  for (i = 0; i < 16; i++)	// { dg-error "the same loop iteration variables 'i' used in multiple associated loops" }
    for (i = 1; i < 32; i++)
      ;
  #pragma omp taskloop collapse(2)
  for (int j = 0; j < 16; j++)	// { dg-error "the same loop iteration variables 'j' used in multiple associated loops" }
    for (j = 0; j < 16; j++)
      ;
}

template <typename T>
void
baz ()
{
  T i = 0;
  #pragma omp for collapse(2)	// { dg-error "the same loop iteration variables 'i' used in multiple associated loops" }
  for (i = 0; i < 16; i++)
    for (i = 1; i < 32; i++)
      ;
  #pragma omp taskloop collapse(2)	// { dg-error "the same loop iteration variables 'j' used in multiple associated loops" }
  for (T j = 0; j < 16; j++)
    for (j = 0; j < 16; j++)
      ;
}

void
test ()
{
  bar <0> ();
  baz <int> ();
}
