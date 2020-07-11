// PR c++/92187
// { dg-do compile { target concepts } }

template <typename>
concept C = false;

C auto f(auto)
{
  return 42; // { dg-error "deduced return type" }
}

void foo()
{
  f(0);
}
