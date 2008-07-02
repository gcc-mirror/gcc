// PR c++/16405
// { dg-options "-O2" } 
// { dg-do run }

// There should be exactly one temporary generated for the code in "f"
// below when optimizing -- for the result of "b + c".  We have no
// easy way of checking that directly, so we count the number of calls
// to "memcpy", which is used on (some?) targets to copy temporaries.
// If there is more than two calls (one for coping "*this" to "t", and
// one for copying the temporary to "a"), then there are too many
// temporaries. 

int i;

extern "C"
void *memcpy (void *dest, const void *src, __SIZE_TYPE__ n)
{
  char *d = (char *) dest;
  const char *s = (const char *) src;
  while (n--)
    d[n] = s[n];
  ++i;
  return dest;
}
 
struct T {
#ifdef __SPU__
  /* SPU returns aggregates up to 1172 bytes in registers.  */
  int a[300];
#else
  int a[128];
#endif
  T &operator+=(T const &v) __attribute__((noinline));
  T operator+(T const &v) const { T t = *this; t += v; return t; }
};

T &T::operator+=(T const &v) {
  return *this;
}

T a, b, c;

void f() { a = b + c; }

int main () {
  i = 0;
  f();
  if (i > 2)
    return 1;
}
