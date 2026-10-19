// Keep implicit zero padding out of constant aggregate copies.
// { dg-do run { target c++14 } }
// { dg-options "-O1 -fdump-tree-gimple" }

struct Buffer
{
  unsigned before;
  char data[4096];
  unsigned after;
};

constexpr Buffer make_buffer ()
{
  Buffer b = {};
  b.before = 123;
  b.data[0] = 'a';
  b.data[2] = 'b';
  b.data[4] = 'c';
  b.after = 456;
  return b;
}

__attribute__((noipa)) Buffer foo ()
{
  constexpr auto b = make_buffer ();
  return b;
}

// A small amount of zero padding should not cause a mostly nonzero aggregate
// to be cleared and initialized element by element.
struct Dense
{
  unsigned values[64];
  char label[32];
};

constexpr Dense make_dense ()
{
  Dense b = {};
  for (unsigned i = 0; i < 64; ++i)
    b.values[i] = i + 1;
  b.label[0] = 'a';
  b.label[1] = 'b';
  b.label[2] = 'c';
  return b;
}

__attribute__((noipa)) Dense bar ()
{
  constexpr auto b = make_dense ();
  return b;
}

int main ()
{
  auto b = foo ();
  const char expected[] = { 'a', 0, 'b', 0, 'c' };
  if (b.before != 123 || b.after != 456)
    __builtin_abort ();
  for (unsigned i = 0; i < sizeof b.data; ++i)
    if (b.data[i] != (i < sizeof expected ? expected[i] : 0))
      __builtin_abort ();

  auto d = bar ();
  for (unsigned i = 0; i < 64; ++i)
    if (d.values[i] != i + 1)
      __builtin_abort ();
  const char label[] = "abc";
  for (unsigned i = 0; i < sizeof d.label; ++i)
    if (d.label[i] != (i < sizeof label ? label[i] : 0))
      __builtin_abort ();
}

// Clear only the sparse aggregate instead of copying its zero padding.
// { dg-final { scan-tree-dump-times "<retval> = {}" 1 "gimple" } }
