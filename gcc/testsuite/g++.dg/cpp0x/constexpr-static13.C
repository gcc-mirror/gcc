// PR c++/92003
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }
// { dg-prune-output "narrowing conversion" }

constexpr char const* get_c_str() { return "abc"; }
constexpr bool use_get_c_str_in_constexpr_context{get_c_str()}; // works

template <char... Cs>
struct string {
  static constexpr char const* c_str() { return c; }

 private:
  static constexpr char c[]{Cs..., '\0'};
};

constexpr char const* cstr{string<'a', 'b', 'c'>::c_str()};
constexpr bool use_cstr_in_constexpr_context{cstr}; // doesn't work
