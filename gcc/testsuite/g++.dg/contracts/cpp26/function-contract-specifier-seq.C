// generic function-contract-specifier-seq parsing checks
// N5008
// function-contract-specifier-seq :
//  	function-contract-specifier function-contract-specifier-seq opt
// function-contract-specifier :
//	precondition-specifier
// 	postcondition-specifier
// precondition-specifier : pre attribute-specifier-seq opt ( conditional-expression )
// postcondition-specifier :
//	post attribute-specifier-seq opt ( result-name-introducer opt conditional-expression )
// { dg-do compile { target c++23 } }
// { dg-additional-options "-fcontracts" }
// { dg-additional-options "-fcontracts" }

static_assert (__cpp_contracts >= 202502L);

void f(int i) pre (i>3);

int g(int j) post (r:r>4) pre (j !=0);

namespace parsing_basic_test {
  int f(const int x)
  pre (x >= 0)
  post (r: r > x); // r is the return value

  struct A {
    bool empty() const noexcept;
    void clear()
      post (empty()); // return value is optional
  };
}

namespace parsing_trailing_return_type_test {
  auto f() -> int pre(true);

  struct S1 {
    auto g() const & noexcept -> int
      pre(true);
  };
}


namespace parsing_lambda_test {
  auto f1 = [] (int x)
    pre(x > 0) { return x * x; };

  auto f2 = [] (int x) -> int
    pre(x > 0) { return x * x; };
}

namespace parsing_pre_post_names_test {
  void f(bool pre, bool post)
  pre(pre) pre(post);
}

namespace parsing_requires_clause_test {
  template <typename T> concept C = true;

  template <typename T>
  auto g(T x) -> bool
    requires C<T>
    pre (x > 0);
}

namespace parsing_ambig1_test {
  const bool a = true;
  using pre = int;

  auto f() -> pre pre(a);
  // pre is the return type, pre(a) the precondition
}

namespace parsing_ambig2_test {
  const bool a = true;
  template <typename T> struct pre {};
  using post = int;

  auto g() -> pre<post> pre(a);
  // pre<post> is the return type, pre(a) the precondition
}

namespace parsing_ambig3_test {
  constexpr bool a = true;
  constexpr bool pre(bool) { return true; }

  template <typename T>
  void f1() requires (pre(a));
  // just a requires clause, no postcondition

  using b = bool;

  template <typename T>
  void f2() requires ((b)pre(a));
  // just a requires clause, no postcondition

  template <typename T>
  void f3() requires (a) pre(a);
  // pre(a) is a postcondition
}

namespace parsing_ambig_test4 {
  constexpr int a = 1;
  constexpr int b = 2;
  constexpr int c = 0;
  constexpr int pre(int i) { return i; }

  template <typename T>
  void f() requires (a < b > pre(c));
  // just a requires clause, no postcondition

  template <typename T> constexpr bool d = true;
  using e = char;

  template <typename T>
  void g() requires d < e > pre(c);
  // d<e> is a bool variable template, pre(c) is the precondition


  template <typename T>
  void f() requires (a < b > pre(c));
  // just a requires clause, no postcondition
}
