// { dg-do assemble  }

enum E { };

template <const E* ep>
struct S1
{
};


struct S2
{
  static E es[1];
};


struct S3
{
  typedef S1<S2::es> S3_Type;
};
