// PR c++/89796
// { dg-do compile }
// { dg-additional-options "-Wunused-value" }

int
f1 (int &c)
{
  int r;
  #pragma omp atomic capture	// { dg-bogus "value computed is not used" }
  { r = c; c++; }
  return r;
}

template <int N>
int
f2 (int &c)
{
  int r;
  #pragma omp atomic capture	// { dg-bogus "value computed is not used" }
  { r = c; c++; }
  return r;
}

int
f3 (int &c)
{
  return f2 <0> (c);
}

int
f4 (int *p)
{
  int r;
  #pragma omp atomic capture	// { dg-bogus "value computed is not used" }
  { r = *p; (*p)++; }
  return r;
}

template <int N>
int
f5 (int *p)
{
  int r;
  #pragma omp atomic capture	// { dg-bogus "value computed is not used" }
  { r = *p; (*p)++; }
  return r;
}

int
f6 (int *p)
{
  return f5 <0> (p);
}
