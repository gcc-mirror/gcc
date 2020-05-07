// PR c++/94938 - ICE in value_dependent_expression_p in C++98 mode.
// { dg-do compile }

template <typename> struct S { S(); S(bool); };

struct C {
  bool operator()(S<float>);
};

S<float> fn (bool);

template<typename T> void
foo (T)
{
  S<float> s;
  S<float> x = fn(false || C()(s));
}

int main ()
{
  foo(int());
}
