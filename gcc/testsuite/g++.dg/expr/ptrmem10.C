/* [expr.eq] If both refer to (possibly different) members of the same union
   (12.3), they compare equal. */
// { dg-do run { target c++11 } }
// { dg-additional-options -O }

union U
{
  int i;
  int j;
};

#define SA(X) static_assert ((X),#X)
SA (&U::i == &U::j);
SA (!(&U::i != &U::j));

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

void f (int U::*p, int U::*q)
{
  assert (p==q);
  assert (!(p!=q));
}

int main()
{
  assert (&U::i == &U::j);
  assert (!(&U::i != &U::j));
}
