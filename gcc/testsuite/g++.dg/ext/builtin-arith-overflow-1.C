// { dg-do compile }

enum A { B = 1, C = 2, D = __builtin_add_overflow_p (B, C, C) };
int e[__builtin_add_overflow_p (B, C, C) + 1];
template <int N> int foo (int);

void
bar ()
{
  foo <__builtin_add_overflow_p (B, C, C) + 1> (0);
}
