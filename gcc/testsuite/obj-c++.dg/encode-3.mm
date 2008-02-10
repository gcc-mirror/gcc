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

#ifdef __LP64__
#define L "q"
#else
#define L "l"
#endif

int main(void) {
  const char *encode = @encode(long);

  if (strcmp (encode, L))
    abort();

  if (strcmp (enc, "{Vec<float>=ff" L "q}"))
    abort();

  if (strcmp (enc2, "{Vec<double>=dd" L "q}"))
    abort();

  return 0;
}
