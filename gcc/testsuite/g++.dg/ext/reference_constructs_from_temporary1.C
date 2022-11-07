// P2255R2
// PR c++/104477
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

struct A { A(); };
struct B { operator A(); };
struct C { explicit C(); };
struct D { explicit operator A(); };
struct E { explicit operator A&(); };
struct F { explicit operator A&&(); };
// Could use a class template with explicit(bool), but then this would need
// C++20.
struct G {
  operator int();
  explicit operator int&&();
};
struct G2 {
  explicit operator int();
  operator int&&();
};
struct H {
  operator int();
  explicit operator int&();
};
struct H2 {
  explicit operator int();
  operator int&();
};

struct Base { };
struct Der : Base { };

template<typename T, typename RT>
struct morph {
  mutable T val{};
  operator RT() const { return static_cast<RT>(val); }
};

template<typename T> using id = T;

// Built-in types.
SA(!__reference_constructs_from_temporary(int, int));
SA(!__reference_constructs_from_temporary(int&, void));
SA(!__reference_constructs_from_temporary(int&, const void));
SA(!__reference_constructs_from_temporary(int&, volatile void));
SA(!__reference_constructs_from_temporary(int&, const volatile void));
SA(!__reference_constructs_from_temporary(void, void));
SA(!__reference_constructs_from_temporary(void, int));
SA(!__reference_constructs_from_temporary(int&, int));
SA(!__reference_constructs_from_temporary(int&, int&));
SA(!__reference_constructs_from_temporary(int&, int&&));
SA(!__reference_constructs_from_temporary(int&, long));
// non-const lvalue reference to type 'int' cannot bind to a value of unrelated type 'long'
SA(!__reference_constructs_from_temporary(int&, long&));
SA(!__reference_constructs_from_temporary(int&, long&&));
SA( __reference_constructs_from_temporary(const int&, int));
SA(!__reference_constructs_from_temporary(const int&, int&));
SA(!__reference_constructs_from_temporary(const int&, const int&));
SA(!__reference_constructs_from_temporary(const int&, int&&));
SA( __reference_constructs_from_temporary(const int&, long));
SA( __reference_constructs_from_temporary(const int&, long&));
SA( __reference_constructs_from_temporary(const int&, long&&));
SA( __reference_constructs_from_temporary(int&&, int));
SA(!__reference_constructs_from_temporary(int&&, int&));
SA(!__reference_constructs_from_temporary(int&&, int&&));
SA( __reference_constructs_from_temporary(int&&, long));
SA( __reference_constructs_from_temporary(int&&, long&));
SA( __reference_constructs_from_temporary(int&&, long&&));
SA(!__reference_constructs_from_temporary(unsigned int&, double));
SA(!__reference_constructs_from_temporary(volatile int&, int));
SA(!__reference_constructs_from_temporary(const volatile int&, int));
SA(!__reference_constructs_from_temporary(volatile int&, int&));
SA(!__reference_constructs_from_temporary(const volatile int&, int&));
SA(!__reference_constructs_from_temporary(volatile int&, int&&));
SA(!__reference_constructs_from_temporary(const volatile int&, int&&));

// Classes.
SA(!__reference_constructs_from_temporary(A, A));
// A& r(A{}); doesn't construct.
SA(!__reference_constructs_from_temporary(A&, A));
SA(!__reference_constructs_from_temporary(A&, A&));
SA(!__reference_constructs_from_temporary(A&, A&&));
// Here we get const struct A & r = (const struct A &) &D.2414;
SA( __reference_constructs_from_temporary(const A&, A));
SA(!__reference_constructs_from_temporary(const A&, A&));
SA(!__reference_constructs_from_temporary(const A&, const A&));
SA(!__reference_constructs_from_temporary(const A&, A&&));
// Here we get struct A & r = (struct A &) &D.2439;
SA( __reference_constructs_from_temporary(A&&, A));
SA(!__reference_constructs_from_temporary(A&&, A&));
SA(!__reference_constructs_from_temporary(A&&, const A&));
SA(!__reference_constructs_from_temporary(A&&, A&&));

SA(!__reference_constructs_from_temporary(A, B));
SA(!__reference_constructs_from_temporary(A&, B));
SA(!__reference_constructs_from_temporary(A&, B&));
SA(!__reference_constructs_from_temporary(A&, const B&));
SA(!__reference_constructs_from_temporary(A&, B&&));
SA( __reference_constructs_from_temporary(const A&, B));
SA( __reference_constructs_from_temporary(const A&, B&));
// Doesn't construct, so it's false.
SA(!__reference_constructs_from_temporary(const A&, const B&));
SA( __reference_constructs_from_temporary(const A&, B&&));
SA( __reference_constructs_from_temporary(A&&, B));
SA( __reference_constructs_from_temporary(A&&, B&));
SA(!__reference_constructs_from_temporary(A&&, const B&));
SA( __reference_constructs_from_temporary(A&&, B&&));

SA(!__reference_constructs_from_temporary(const A&, C));
SA(!__reference_constructs_from_temporary(const A&, C&));

// Doesn't construct, so it's false.
SA(!__reference_constructs_from_temporary(int&, morph<int, int>));
// Here we get
//   const int & r2 = D.2580 = morph<int, int>::operator int
//     (&TARGET_EXPR <D.2578, {.val=0}>); (const int &) &D.2580;
SA( __reference_constructs_from_temporary(const int&, morph<int, int>));
SA(!__reference_constructs_from_temporary(int&, morph<int, int&>));
SA(!__reference_constructs_from_temporary(int&, morph<int, const int&>));
SA(!__reference_constructs_from_temporary(int&, morph<int, int&&>));
SA( __reference_constructs_from_temporary(const int&, morph<long, long&>));

// These are like const C& c(cref); so the explicit ctor C isn't a problem
// even in copy-init context.  const C& r = {}; would be a different story.
SA(!__reference_constructs_from_temporary(C, C));
SA(!__reference_constructs_from_temporary(C&, C));
SA(!__reference_constructs_from_temporary(C&, C&));
SA(!__reference_constructs_from_temporary(C&, C&&));
SA( __reference_constructs_from_temporary(const C&, C));
SA(!__reference_constructs_from_temporary(const C&, C&));
SA(!__reference_constructs_from_temporary(const C&, const C&));
SA(!__reference_constructs_from_temporary(const C&, C&&));
SA( __reference_constructs_from_temporary(C&&, C));
SA(!__reference_constructs_from_temporary(C&&, C&));
SA(!__reference_constructs_from_temporary(C&&, const C&));
SA(!__reference_constructs_from_temporary(C&&, C&&));

// These are all false ultimately because of CWG 2267, which we implement.
SA(!__reference_constructs_from_temporary(A, D));
SA(!__reference_constructs_from_temporary(A&, D));
SA(!__reference_constructs_from_temporary(A&, D&));
SA(!__reference_constructs_from_temporary(A&, const D&));
SA(!__reference_constructs_from_temporary(A&, D&&));
SA(!__reference_constructs_from_temporary(const A&, D));
SA(!__reference_constructs_from_temporary(const A&, D&));
SA(!__reference_constructs_from_temporary(const A&, const D&));
SA(!__reference_constructs_from_temporary(const A&, D&&));
SA(!__reference_constructs_from_temporary(A&&, D));
SA(!__reference_constructs_from_temporary(A&&, D&));
SA(!__reference_constructs_from_temporary(A&&, const D&));
SA(!__reference_constructs_from_temporary(A&&, D&&));

SA(!__reference_constructs_from_temporary(A, E));
/* A& a1(E{}); compiles, but A& a2 = E{}; doesn't.
   With the former, we get A& a = E::operator A& (&TARGET_EXPR <D.2715, {}>)
   so we're not binding the reference to a temporary, although there is
   a temporary involved.  So the result is false in both copy- and direct-
   init, albeit for different reasons!  */
SA(!__reference_constructs_from_temporary(A&, E));
// A& a = E::operator A& ((struct E *) r)); copy-init doesn't compile.
SA(!__reference_constructs_from_temporary(A&, E&));
SA(!__reference_constructs_from_temporary(A&, const E&));
SA(!__reference_constructs_from_temporary(A&, E&&));
// direct-init:
// const A& a = (const struct A &) E::operator A& (&TARGET_EXPR <D.2720, {}>)
SA(!__reference_constructs_from_temporary(const A&, E));
SA(!__reference_constructs_from_temporary(const A&, E&));
SA(!__reference_constructs_from_temporary(const A&, const E&));
SA(!__reference_constructs_from_temporary(const A&, E&&));
SA(!__reference_constructs_from_temporary(A&&, E));
SA(!__reference_constructs_from_temporary(A&&, E&));
SA(!__reference_constructs_from_temporary(A&&, const E&));
SA(!__reference_constructs_from_temporary(A&&, E&&));

SA(!__reference_constructs_from_temporary(A, F));
// A& a1(F{}); and A& a2 = F{}; both invalid.
SA(!__reference_constructs_from_temporary(A&, F));
SA(!__reference_constructs_from_temporary(A&, F&));
SA(!__reference_constructs_from_temporary(A&, const F&));
SA(!__reference_constructs_from_temporary(A&, F&&));
SA(!__reference_constructs_from_temporary(const A&, F));
SA(!__reference_constructs_from_temporary(const A&, F&));
SA(!__reference_constructs_from_temporary(const A&, const F&));
SA(!__reference_constructs_from_temporary(const A&, F&&));
SA(!__reference_constructs_from_temporary(A&&, F));
SA(!__reference_constructs_from_temporary(A&&, F&));
SA(!__reference_constructs_from_temporary(A&&, const F&));
SA(!__reference_constructs_from_temporary(A&&, F&&));

/* This is where _converts_ and _constructs_ will differ:
   in direct-init we use G::operator int&& (no temporary),
   but in copy-init we use G::operator int, where a temporary is created
   to be bound to int&&.  */
SA(!__reference_constructs_from_temporary(int&&, G));
// Similar to the previous one.
SA(!__reference_constructs_from_temporary(const int&, H));
/* And here I've switched the explicit-ness.  In both copy- and direct-init
   we call operator int&, so no temporary.  */
SA(!__reference_constructs_from_temporary(int&&, G2));
SA(!__reference_constructs_from_temporary(const int&, H2));

SA(__reference_constructs_from_temporary(const Base&, Der));

// This fails because std::is_constructible_v<int&&, id<int[3]>> is false.
SA(!__reference_constructs_from_temporary(int&&, id<int[3]>));

// Arrays.
SA(!__reference_constructs_from_temporary(int, int[]));
SA(!__reference_constructs_from_temporary(int[], int[]));
SA(!__reference_constructs_from_temporary(int&, int[]));
SA(!__reference_constructs_from_temporary(int&&, int[]));
SA(!__reference_constructs_from_temporary(const int&, int[]));
