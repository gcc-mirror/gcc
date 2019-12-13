// Test lambda mangling
// { dg-do compile { target c++11 } }
// { dg-require-weak "" }
// { dg-options "-fno-inline" }

template<typename F> int algo(F fn) { return fn(); }
inline void g(int n) {
  int bef(int i = []{ return 1; }());
  // Default arguments of block-extern function declarations
  // remain in the context of the encloding function body.
  // The closure type is encoded as Z1giEUlvE_.
  // The call operator of that type is _ZZ1giENKUlvE_clEv.

// { dg-final { scan-assembler "_ZZ1giENKUlvE_clEv" } }
// { dg-final { scan-assembler "weak\[^\n\r\]*_?_ZZ1giENKUlvE_clEv" { target { ! { *-*-darwin* *-*-mingw* *-*-cygwin } } } } }

  algo([=]{return n+bef();});
  // The captured entities do not participate in <lambda-sig>
  // and so this closure type has the same <lambda-sig> as
  // the previous one.  It encoding is therefore Z1giEUlvE0_
  // and the call operator is _ZZ1giENKUlvE0_clEv.  The
  // instance of "algo" being called is then
  // _Z4algoIZ1giEUlvE0_EiT_.

// { dg-final { scan-assembler "_Z4algoIZ1giEUlvE0_EiT_" } }
// { dg-final { scan-assembler "_ZZ1giENKUlvE0_clEv" } }

  int i = []{return 1;}();

}

struct S {
  void f(int =
	 // Type: ZN1S1fEiiEd0_UlvE_
	 // Operator: _ZZN1S1fEiiEd0_NKUlvE_clEv
// { dg-final { scan-assembler "_ZZN1S1fEiiEd0_NKUlvE_clEv" } }
// { dg-final { scan-assembler "weak\[^\n\r\]*_?_ZZN1S1fEiiEd0_NKUlvE_clEv" { target { ! { *-*-darwin* *-*-mingw* *-*-cygwin } } } } }
	 []{return 1;}()
	 // Type: ZN1S1fEiiEd0_UlvE0_
	 // Operator: _ZZN1S1fEiiEd0_NKUlvE0_clEv
// { dg-final { scan-assembler "_ZZN1S1fEiiEd0_NKUlvE0_clEv" } }
	 + []{return 2;}(),
	 int =
	 // Type: ZN1S1fEiiEd_UlvE_
	 // Operator: _ZZN1S1fEiiEd_NKUlvE_clEv
// { dg-final { scan-assembler "_ZZN1S1fEiiEd_NKUlvE_clEv" } }
	 []{return 3;}());
};

void bar()
{
  // lambdas in non-vague linkage functions have internal linkage.
  // { dg-final { scan-assembler-not "weak\[^\n\r\]*bar\[^\n\r\]*Ul" } }
  []{}();
}

// lambdas used in non-template, non-class body initializers are internal.
// { dg-final { scan-assembler-not "weak\[^\n\r\]*_ZNKUlv" } }
// { dg-final { scan-assembler-not "weak\[^\n\r\]*variable" } }
int variable = []{return 1;}();

// And a template instantiated with such a lambda is also internal.
// { dg-final { scan-assembler-not "weak\[^\n\r\]*algoIUl" } }
int var2 = algo([]{return 1;});

// As are lambdas used in non-class-body default arguments.
// { dg-final { scan-assembler-not "weak\[^\n\r\]*function" } }
void function (int i = []{return 1;}()+[]{return 1;}());

struct Foo
{
  static int Int;
  void Bar(int);
};

int Foo::Int = []{return 1;}();
// Even default arguments for member functions that appear outside the
// class body are internal.
// { dg-final { scan-assembler-not "weak\[^\n\r\]*Foo" } }
void Foo::Bar(int i = []{return 1;}()) {}

// Even default arguments for function templates.
// { dg-final { scan-assembler-not "weak\[^\n\r\]*fn2\[^\n\r\]*Ulv" } }
template <class T>
void fn2 (T t = []{return 1;}()) {}

int main()
{
  g(42);
  S().f();
  function();
  Foo().Bar();
  fn2<int>();
}
