// PR c++/99895
// { dg-do compile { target c++20 } }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

struct fixed_string {
  consteval int size(int n) const {
    if (n < 0) throw; // { dg-error "not a constant" }
    return n;
  }

  static consteval int size_static(int n) {
    if (n < 0) throw; // { dg-error "not a constant" }
    return n;
  }

  consteval void operator()() const { }
};

template<class>
void VerifyHash(fixed_string s) {
  s.size(0); // { dg-bogus "" }
  s.size(-1); // { dg-message "expansion of" }
  s.size_static(0); // { dg-bogus "" }
  s.size_static(-1); // { dg-message "expansion of" }
  fixed_string::size_static(0); // { dg-bogus "" }
  fixed_string::size_static(-1); // { dg-message "expansion of" }
  s(); // { dg-bogus "" }
}

void
do_test ()
{
  fixed_string f;
  VerifyHash<int>(f);
}
