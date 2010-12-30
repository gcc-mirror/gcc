// PR c++/42083
// { dg-options "-std=c++0x" }

template<typename F>
decltype(F()) run(F f) // { dg-message "note" }
{
  return f();
}

int main()
{
  auto l = []() { return 5; };

  run(l); // { dg-error "no match" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 14 }
}
