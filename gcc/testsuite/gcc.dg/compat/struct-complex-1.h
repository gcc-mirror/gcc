#include <complex.h>

struct st
{
  int s1;
  float complex x;
  int s2;
};

typedef struct { float r, i; } _complex;

struct stc
{
  int s1;
  _complex x;
  int s2;
};
