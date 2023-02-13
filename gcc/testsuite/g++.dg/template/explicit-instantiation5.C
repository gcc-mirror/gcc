// PR c++/108496

struct S { long a, b, c; } s;

template <int, typename>
S foo (S);

template <>
S
foo <0, long> (S)
{
  return s;
}

template S foo <0, long> (S);
