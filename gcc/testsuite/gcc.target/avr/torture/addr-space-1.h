#include <stdlib.h>
#include <string.h>

typedef struct
{
  char i1;
  short i2;
  long i4;
  long long i8;
  char str[2][10];
} a_t;

const __as a_t A =
  {
    12, 345, 678910, 1234567891011ll,
    {
      "xxx..xxx",
      "yyy..yyy"
    }
  };

const __as volatile a_t V =
  {
    12+1, 345+1, 678910+1, 1234567891011ll+1,
    {
      "XXX..XXX",
      "YYY..YYY"
    }
  };

a_t A2;
volatile a_t V2;

int main (void)
{
  if (A.i1 != 12
      || A.i1 != V.i1 -1)
    abort();

  if (A.i2 != 345
      || A.i2 != V.i2 -1)
    abort();

  if (A.i4 != 678910
      || A.i4 != V.i4 -1)
    abort();

  if (A.i8 != 1234567891011ll
      || A.i8 != V.i8 -1)
    abort();

  A2 = A;
  V2 = V;

  if (A2.i1 != 12
      || A2.i1 != V2.i1 -1)
    abort();

  if (A2.i2 != 345
      || A2.i2 != V2.i2 -1)
    abort();

  if (A2.i4 != 678910
      || A2.i4 != V2.i4 -1)
    abort();

  if (A2.i8 != 1234567891011ll
      || A2.i8 != V2.i8 -1)
    abort();

  if (strcmp (A2.str[0], "xxx..xxx"))
    abort();
  if (strcmp (A2.str[1], "yyy..yyy"))
    abort();

  if (strcmp ((const char*) V2.str[0], "XXX..XXX"))
    abort();
  if (strcmp ((const char*) V2.str[1], "YYY..YYY"))
   abort();
  
  exit (0);
  return 0;
}
