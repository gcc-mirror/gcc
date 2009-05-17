// Test ICE in caching printable names for a function.
// { dg-options "-std=c++98 -pedantic -O2" }

void g (int a) __attribute__((warning("g")));
void g2 (int a, int *p);
static inline __attribute__((__always_inline__)) void
gg (int a)
{
  if (a == 0)
    return g(a); // { dg-warning "attribute" }
  __extension__ int v[a];
  return g2(a, v);
}

void h (int a) __attribute__((warning("h")));
void h2 (int a, int *p);
static inline __attribute__((__always_inline__)) void
hh (int a)
{
  if (a == 0)
    return h(a); // { dg-warning "attribute" }
  __extension__ int v[a];
  return h2(a, v);
}

void i (int a) __attribute__((warning("i")));
void i2 (int a, int *p);
static inline __attribute__((__always_inline__)) void
ii (int a)
{
  if (a == 0)
    return i(a); // { dg-warning "attribute" }
  __extension__ int v[a];
  return i2(a, v);
}

void
f (void)
{
  long long l; // { dg-warning "long long" }
  const char *p = __PRETTY_FUNCTION__;
  gg(0);
  hh(0);
  ii(0);
}
