/* { dg-do run } */

#include <stdlib.h>
#include <string.h>

template <class T>
struct Vec {
 T x, y;
 long z;
 long long zz;
};

Vec<double> dd;
const char *enc = @encode(Vec<float>);
const char *enc2 = @encode(Vec<double>);

int main(void) {
  char *encode = @encode(long);

  if (strcmp (encode, "l"))
    abort();

  if (strcmp (enc, "{Vec<float>=fflq}"))
    abort();

  if (strcmp (enc2, "{Vec<double>=ddlq}"))
    abort();

  return 0;
}
