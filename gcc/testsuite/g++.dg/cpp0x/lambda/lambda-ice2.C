// PR c++/42083
// { dg-do compile { target c++11 } }

template<typename F>
decltype(F()) run(F f) // { dg-message "note" }
{
  return f();
}

int main()
{
  auto l = []() { return 5; }; // { dg-message "lambda closure type" }

  run(l); // { dg-error "no match" }
  // { dg-error "use of deleted function" "candidate explanation" { target *-*-* } 5 }
}
