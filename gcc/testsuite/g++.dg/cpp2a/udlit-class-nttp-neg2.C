// PR c++/88095
// Test class non-type template parameters for literal operator templates.
// Validate that non-literal class types are rejected.
// { dg-do compile { target c++20 } }

struct non_literal_class {
  constexpr non_literal_class(...) { }
  ~non_literal_class() {}
  // auto operator<=> (const non_literal_fixed_string&) = default;
};

template <non_literal_class> // { dg-error "11:is not a valid type for a template non-type parameter because it is not structural" }
int operator"" _udl();       // { dg-error "5:literal operator template .int operator\"\"_udl\\(\\). has invalid parameter list" }
