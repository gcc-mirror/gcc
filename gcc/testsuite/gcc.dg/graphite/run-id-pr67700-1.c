#include <stdlib.h>
#include <assert.h>

struct type *obj;
struct type {
  int elem1[81];
};

enum fpmath_unit
{
  FPMATH_387 = 1,
  FPMATH_SSE = 2
};

struct gcc_options
{
  enum fpmath_unit x_ix86_fpmath;
};

struct gcc_options global_options;

void foo(void)
{
   int pos = 0;
   int i;
   if (!((global_options.x_ix86_fpmath & FPMATH_SSE) != 0))
     for (i = 8; i <= 15; i++)
       (obj->elem1) [pos++] = i;
   for (i = 45; i <= 52; i++)
     (obj->elem1) [pos++] = i;
   if (((global_options.x_ix86_fpmath & FPMATH_SSE) != 0))
     for (i = 8; i <= 15; i++)
       (obj->elem1) [pos++] = i;
   for (i = 29; i <= 36; i++)
     (obj->elem1) [pos++] = i;
}

int main()
{
  int i;
  obj = (struct type*) malloc (sizeof (struct type));
  for (i = 0; i <= 80; i++)
    obj->elem1[i] = 0;
  foo();
  assert (obj->elem1[8] == 45);
  return 0;
}

