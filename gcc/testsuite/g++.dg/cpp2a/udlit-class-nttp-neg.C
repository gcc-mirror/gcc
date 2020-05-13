// PR c++/88095
// Test class non-type template parameters for literal operator templates.
// Validate that parameter packs are rejected.
// { dg-do compile { target c++20 } }

struct literal_class {
  constexpr literal_class(...) { }
  // auto operator<=> (const fixed_string&) = default;
};

template <literal_class...>
int operator"" _udl();      // { dg-error "5:literal operator template .int operator\"\"_udl\\(\\). has invalid parameter list" }
