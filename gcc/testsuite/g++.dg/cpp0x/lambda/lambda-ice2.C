// PR c++/42083
// { dg-do compile { target c++11 } }

template<typename F>
decltype(F()) run(F f) // { dg-message "note" "" { target c++17_down } }
{
  return f();	// { dg-error "could not convert" "" { target c++2a } }
}

int main()
{
  auto l = []() { return 5; }; // { dg-message "lambda closure type" "" { target c++17_down } }

  run(l); // { dg-error "no match" "" { target c++17_down } }
  // { dg-error "use of deleted function" "candidate explanation" { target c++17_down } 5 }
}
