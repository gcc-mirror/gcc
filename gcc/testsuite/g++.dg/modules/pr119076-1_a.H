// { dg-additional-options "-fmodule-header -Wno-pedantic" }
// { dg-module-cmi {} }

constexpr unsigned char a[] = {
#embed __FILE__
};

constexpr int
foo (const unsigned char *p, int s)
{
  int r = 0;
  for (int i = 0; i < s; ++i)
    r += p[i];
  return r;
}

constexpr int b = foo (a, sizeof a);

inline int
bar ()
{
  const unsigned char b[] = {
    #embed __FILE__
  };
  int r = 0;
  for (int i = 0; i < sizeof b; ++i)
    r += b[i];
  return r;
}

inline int
baz ()
{
  const long int c[] = {
    #embed __FILE__
  };
  int r = 0;
  for (int i = 0; i < sizeof (c) / sizeof (c[0]); ++i)
    r += c[i];
  return r;
}
