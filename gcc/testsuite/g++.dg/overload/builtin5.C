// { dg-do compile { target c++17 } }
// Don't add built-in operator for ++ on bool.

template<typename T>
struct S { operator T&(); };

template<int> void
foo (S<bool>& s)
{
  --s; // { dg-error "no match for" }
  ++s; // { dg-error "no match for" }
  s++; // { dg-error "declared for postfix" }
  s--; // { dg-error "declared for postfix" }
}

void
bar ()
{
  S<bool> s;
  foo<0> (s);
}
