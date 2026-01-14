// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test that we give a nice error about a missing typename.

struct S {
  using type = int;
};

template<typename T>
void
f ()
{
  [: ^^T :]::type i = 42; // { dg-error "need .typename. before .\\\[: \\\^\\\^T :\\\]::type. because .\\\[: \\\^\\\^T :\\\]. is a dependent scope" }
}

void
g ()
{
  f<S>();
}
