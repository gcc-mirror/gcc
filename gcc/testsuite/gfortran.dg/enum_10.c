/* This testcase is meant to be compiled together with enum_10.f90  */

extern void abort (void);

typedef enum
  { MAX1 = 127 } onebyte;

void f1_ (onebyte *i, int *j)
{
  if (*i != *j) abort ();
}

typedef enum
  { MAX2 = 32767 } twobyte;

void f2_ (twobyte *i, int *j)
{
  if (*i != *j) abort ();
}

typedef enum
  { MAX4 = 2000000 } fourbyte; /* don't need the precise value. */

void f4_ (fourbyte *i, int *j)
{
  if (*i != *j) abort ();
}
