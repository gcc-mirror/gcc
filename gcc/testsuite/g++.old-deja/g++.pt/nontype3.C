// Build don't link:

enum E { e };

template <const E* ep>
struct S1
{
  static char* s;
};

template <int D>
struct S2 {};

template <>
struct S2<1>
{
  static E es[1];
};

struct S3
{
  typedef S1<S2<1>::es> S3_Type;
};

E S2<1>::es[1] = {e};

template <>
char* S1<S2<1>::es>::s = "abc";
