// { dg-do compile }
// { dg-options "-Wall" }

typedef __SIZE_TYPE__ size_t;
extern "C" void *memset (void *, int, size_t);
char buf[1024];
namespace std
{
  extern "C" void *memset (void *, int, size_t);
}

template <int N>
void
foo ()
{
  memset (buf, sizeof buf, 0);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, '\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, L'\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, N);
  memset (buf, 1, 1 - 1);
  memset (buf, 1, 0 - 0);
  memset (buf, 1, N - N);
  memset (buf, 0, 0);
  memset (buf, 1 - 1, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, 0 - 0, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0L);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0UL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0LL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, 0ULL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  memset (buf, sizeof buf, (int) 0);
  memset (buf, sizeof buf, -0);
}

template <int N>
void
baz ()
{
  std::memset (buf, sizeof buf, 0);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, '\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, L'\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, N);
  std::memset (buf, 1, 1 - 1);
  std::memset (buf, 1, 0 - 0);
  std::memset (buf, 1, N - N);
  std::memset (buf, 0, 0);
  std::memset (buf, 1 - 1, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, 0 - 0, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0L);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0UL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0LL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0ULL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, (int) 0);
  std::memset (buf, sizeof buf, -0);
}

void
bar ()
{
  foo<0> ();
  std::memset (buf, sizeof buf, 0);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, '\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, L'\0'); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, 1, 1 - 1);
  std::memset (buf, 1, 0 - 0);
  std::memset (buf, 0, 0);
  std::memset (buf, 1 - 1, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, 0 - 0, 0); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0L);	/* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0UL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0LL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, 0ULL); /* { dg-warning ".memset. used with constant zero length parameter; this could be due to transposed parameters" } */
  std::memset (buf, sizeof buf, (int) 0);
  std::memset (buf, sizeof buf, -0);
}
