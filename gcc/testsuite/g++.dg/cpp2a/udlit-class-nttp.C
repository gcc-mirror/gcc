// PR c++/88095
// Test class non-type template parameters for literal operator templates.
// Validate basic support.
// { dg-do compile { target c++2a } }

struct literal_class {
  constexpr literal_class(...) { }
  // auto operator<=> (const fixed_string&) = default;
};

template <literal_class>
constexpr int operator"" _udl() {
  return 1;
}

static_assert("test"_udl == 1);
