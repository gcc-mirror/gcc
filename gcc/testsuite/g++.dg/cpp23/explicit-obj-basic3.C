// P0847R7
// { dg-do compile { target c++23 } }

// bogus diagnosis of valid declarations as redeclarations
// tests for by-value are elsewhere (todo: add filename)

// each group has 8 overloads that each take
// lvalue ref to S
// rvalue ref to S
// lvalue c ref to S
// rvalue c ref to S
// lvalue v ref to S
// rvalue v ref to S
// lvalue cv ref to S
// rvalue cv ref to S
// where S is the struct the function is declared in

// only xobj (the most basic case)

struct S {
  void f(this S &);
  void f(this S &&);
  void f(this S const&);
  void f(this S const&&);
  void f(this S volatile&);
  void f(this S volatile&&);
  void f(this S const volatile&);
  void f(this S const volatile&&);
};

// I* has the 1 xobj 7 iobj cases
// X* has the 7 xobj 1 iobj cases
// *0 has the unique function first, the rest after
// *1 has the unique function last, the rest after
// *2 has the functions in the order stated above
// xobj first, 1 xobj, 7 iobj

// (yes there are some redundant cases)

// unique first, 1 xobj 7 iobj

struct I0 {
  void f0(this I0&);
  void f0() &&;
  void f0() const&;
  void f0() const&&;
  void f0() volatile&;
  void f0() volatile&&;
  void f0() const volatile&;
  void f0() const volatile&&;

  void f1(this I0&&);
  void f1() &;
  void f1() const&;
  void f1() const&&;
  void f1() volatile&;
  void f1() volatile&&;
  void f1() const volatile&;
  void f1() const volatile&&;

  void fc0(this I0 const&);
  void fc0() &;
  void fc0() &&;
  void fc0() const&&;
  void fc0() volatile&;
  void fc0() volatile&&;
  void fc0() const volatile&;
  void fc0() const volatile&&;

  void fc1(this I0 const&&);
  void fc1() &;
  void fc1() &&;
  void fc1() const&;
  void fc1() volatile&;
  void fc1() volatile&&;
  void fc1() const volatile&;
  void fc1() const volatile&&;

  void fv0(this I0 volatile&);
  void fv0() &;
  void fv0() &&;
  void fv0() const&;
  void fv0() const&&;
  void fv0() volatile&&;
  void fv0() const volatile&;
  void fv0() const volatile&&;

  void fv1(this I0 volatile&&);
  void fv1() &;
  void fv1() &&;
  void fv1() const&;
  void fv1() const&&;
  void fv1() volatile&;
  void fv1() const volatile&;
  void fv1() const volatile&&;

  void fcv0(this I0 const volatile&);
  void fcv0() &;
  void fcv0() &&;
  void fcv0() const&;
  void fcv0() const&&;
  void fcv0() volatile&;
  void fcv0() volatile&&;
  void fcv0() const volatile&&;

  void fcv1(this I0 const volatile&&);
  void fcv1() &;
  void fcv1() &&;
  void fcv1() const&;
  void fcv1() const&&;
  void fcv1() volatile&;
  void fcv1() volatile&&;
  void fcv1() const volatile&;
};

// unique last, 1 xobj 7 iobj

struct I1 {
  void f0() &&;
  void f0() const&;
  void f0() const&&;
  void f0() volatile&;
  void f0() volatile&&;
  void f0() const volatile&;
  void f0() const volatile&&;
  void f0(this I1&);

  void f1() &;
  void f1() const&;
  void f1() const&&;
  void f1() volatile&;
  void f1() volatile&&;
  void f1() const volatile&;
  void f1() const volatile&&;
  void f1(this I1&&);

  void fc0() &;
  void fc0() &&;
  void fc0() const&&;
  void fc0() volatile&;
  void fc0() volatile&&;
  void fc0() const volatile&;
  void fc0() const volatile&&;
  void fc0(this I1 const&);

  void fc1() &;
  void fc1() &&;
  void fc1() const&;
  void fc1() volatile&;
  void fc1() volatile&&;
  void fc1() const volatile&;
  void fc1() const volatile&&;
  void fc1(this I1 const&&);

  void fv0() &;
  void fv0() &&;
  void fv0() const&;
  void fv0() const&&;
  void fv0() volatile&&;
  void fv0() const volatile&;
  void fv0() const volatile&&;
  void fv0(this I1 volatile&);

  void fv1() &;
  void fv1() &&;
  void fv1() const&;
  void fv1() const&&;
  void fv1() volatile&;
  void fv1() const volatile&;
  void fv1() const volatile&&;
  void fv1(this I1 volatile&&);

  void fcv0() &;
  void fcv0() &&;
  void fcv0() const&;
  void fcv0() const&&;
  void fcv0() volatile&;
  void fcv0() volatile&&;
  void fcv0() const volatile&&;
  void fcv0(this I1 const volatile&);

  void fcv1() &;
  void fcv1() &&;
  void fcv1() const&;
  void fcv1() const&&;
  void fcv1() volatile&;
  void fcv1() volatile&&;
  void fcv1() const volatile&;
  void fcv1(this I1 const volatile&&);
};

// ordered, 1 xobj 7 iobj

struct I2 {
  void f0(this I2&);
  void f0() &&;
  void f0() const&;
  void f0() const&&;
  void f0() volatile&;
  void f0() volatile&&;
  void f0() const volatile&;
  void f0() const volatile&&;

  void f1() &;
  void f1(this I2&&);
  void f1() const&;
  void f1() const&&;
  void f1() volatile&;
  void f1() volatile&&;
  void f1() const volatile&;
  void f1() const volatile&&;

  void fc0() &;
  void fc0() &&;
  void fc0(this I2 const&);
  void fc0() const&&;
  void fc0() volatile&;
  void fc0() volatile&&;
  void fc0() const volatile&;
  void fc0() const volatile&&;

  void fc1() &;
  void fc1() &&;
  void fc1() const&;
  void fc1(this I2 const&&);
  void fc1() volatile&;
  void fc1() volatile&&;
  void fc1() const volatile&;
  void fc1() const volatile&&;

  void fv0() &;
  void fv0() &&;
  void fv0() const&;
  void fv0() const&&;
  void fv0(this I2 volatile&);
  void fv0() volatile&&;
  void fv0() const volatile&;
  void fv0() const volatile&&;

  void fv1() &;
  void fv1() &&;
  void fv1() const&;
  void fv1() const&&;
  void fv1() volatile&;
  void fv1(this I2 volatile&&);
  void fv1() const volatile&;
  void fv1() const volatile&&;

  void fcv0() &;
  void fcv0() &&;
  void fcv0() const&;
  void fcv0() const&&;
  void fcv0() volatile&;
  void fcv0() volatile&&;
  void fcv0(this I2 const volatile&);
  void fcv0() const volatile&&;

  void fcv1() &;
  void fcv1() &&;
  void fcv1() const&;
  void fcv1() const&&;
  void fcv1() volatile&;
  void fcv1() volatile&&;
  void fcv1() const volatile&;
  void fcv1(this I2 const volatile&&);
};


// iobj first, 7 xobj, 1 iobj

struct X0 {
  void f0() &;
  void f0(this X0 &&);
  void f0(this X0 const&);
  void f0(this X0 const&&);
  void f0(this X0 volatile&);
  void f0(this X0 volatile&&);
  void f0(this X0 const volatile&);
  void f0(this X0 const volatile&&);

  void f1() &&;
  void f1(this X0 &);
  void f1(this X0 const&);
  void f1(this X0 const&&);
  void f1(this X0 volatile&);
  void f1(this X0 volatile&&);
  void f1(this X0 const volatile&);
  void f1(this X0 const volatile&&);

  void fc0() const&;
  void fc0(this X0 &);
  void fc0(this X0 &&);
  void fc0(this X0 const&&);
  void fc0(this X0 volatile&);
  void fc0(this X0 volatile&&);
  void fc0(this X0 const volatile&);
  void fc0(this X0 const volatile&&);

  void fc1() const&&;
  void fc1(this X0 &);
  void fc1(this X0 &&);
  void fc1(this X0 const&);
  void fc1(this X0 volatile&);
  void fc1(this X0 volatile&&);
  void fc1(this X0 const volatile&);
  void fc1(this X0 const volatile&&);

  void fv0() volatile&;
  void fv0(this X0 &);
  void fv0(this X0 &&);
  void fv0(this X0 const&);
  void fv0(this X0 const&&);
  void fv0(this X0 volatile&&);
  void fv0(this X0 const volatile&);
  void fv0(this X0 const volatile&&);

  void fv1() volatile&&;
  void fv1(this X0 &);
  void fv1(this X0 &&);
  void fv1(this X0 const&);
  void fv1(this X0 const&&);
  void fv1(this X0 volatile&);
  void fv1(this X0 const volatile&);
  void fv1(this X0 const volatile&&);

  void fcv0() const volatile&;
  void fcv0(this X0 &);
  void fcv0(this X0 &&);
  void fcv0(this X0 const&);
  void fcv0(this X0 const&&);
  void fcv0(this X0 volatile&);
  void fcv0(this X0 volatile&&);
  void fcv0(this X0 const volatile&&);

  void fcv1() const volatile&&;
  void fcv1(this X0 &);
  void fcv1(this X0 &&);
  void fcv1(this X0 const&);
  void fcv1(this X0 const&&);
  void fcv1(this X0 volatile&);
  void fcv1(this X0 volatile&&);
  void fcv1(this X0 const volatile&);
};

// iobj last, 7 xobj 1 iobj

struct X1 {
  void f0(this X1 &&);
  void f0(this X1 const&);
  void f0(this X1 const&&);
  void f0(this X1 volatile&);
  void f0(this X1 volatile&&);
  void f0(this X1 const volatile&);
  void f0(this X1 const volatile&&);
  void f0() &;

  void f1(this X1 &);
  void f1(this X1 const&);
  void f1(this X1 const&&);
  void f1(this X1 volatile&);
  void f1(this X1 volatile&&);
  void f1(this X1 const volatile&);
  void f1(this X1 const volatile&&);
  void f1() &&;

  void fc0(this X1 &);
  void fc0(this X1 &&);
  void fc0(this X1 const&&);
  void fc0(this X1 volatile&);
  void fc0(this X1 volatile&&);
  void fc0(this X1 const volatile&);
  void fc0(this X1 const volatile&&);
  void fc0() const&;

  void fc1(this X1 &);
  void fc1(this X1 &&);
  void fc1(this X1 const&);
  void fc1(this X1 volatile&);
  void fc1(this X1 volatile&&);
  void fc1(this X1 const volatile&);
  void fc1(this X1 const volatile&&);
  void fc1() const&&;

  void fv0(this X1 &);
  void fv0(this X1 &&);
  void fv0(this X1 const&);
  void fv0(this X1 const&&);
  void fv0(this X1 volatile&&);
  void fv0(this X1 const volatile&);
  void fv0(this X1 const volatile&&);
  void fv0() volatile&;

  void fv1(this X1 &);
  void fv1(this X1 &&);
  void fv1(this X1 const&);
  void fv1(this X1 const&&);
  void fv1(this X1 volatile&);
  void fv1(this X1 const volatile&);
  void fv1(this X1 const volatile&&);
  void fv1() volatile&&;

  void fcv0(this X1 &);
  void fcv0(this X1 &&);
  void fcv0(this X1 const&);
  void fcv0(this X1 const&&);
  void fcv0(this X1 volatile&);
  void fcv0(this X1 volatile&&);
  void fcv0(this X1 const volatile&&);
  void fcv0() const volatile&;

  void fcv1(this X1 &);
  void fcv1(this X1 &&);
  void fcv1(this X1 const&);
  void fcv1(this X1 const&&);
  void fcv1(this X1 volatile&);
  void fcv1(this X1 volatile&&);
  void fcv1(this X1 const volatile&);
  void fcv1() const volatile&&;
};

// ordered, 7 xobj 1 iobj

struct X2 {
  void f0() &;
  void f0(this X2 &&);
  void f0(this X2 const&);
  void f0(this X2 const&&);
  void f0(this X2 volatile&);
  void f0(this X2 volatile&&);
  void f0(this X2 const volatile&);
  void f0(this X2 const volatile&&);

  void f1(this X2 &);
  void f1() &&;
  void f1(this X2 const&);
  void f1(this X2 const&&);
  void f1(this X2 volatile&);
  void f1(this X2 volatile&&);
  void f1(this X2 const volatile&);
  void f1(this X2 const volatile&&);

  void fc0(this X2 &);
  void fc0(this X2 &&);
  void fc0() const&;
  void fc0(this X2 const&&);
  void fc0(this X2 volatile&);
  void fc0(this X2 volatile&&);
  void fc0(this X2 const volatile&);
  void fc0(this X2 const volatile&&);

  void fc1(this X2 &);
  void fc1(this X2 &&);
  void fc1(this X2 const&);
  void fc1() const&&;
  void fc1(this X2 volatile&);
  void fc1(this X2 volatile&&);
  void fc1(this X2 const volatile&);
  void fc1(this X2 const volatile&&);

  void fv0(this X2 &);
  void fv0(this X2 &&);
  void fv0(this X2 const&);
  void fv0(this X2 const&&);
  void fv0() volatile&;
  void fv0(this X2 volatile&&);
  void fv0(this X2 const volatile&);
  void fv0(this X2 const volatile&&);

  void fv1(this X2 &);
  void fv1(this X2 &&);
  void fv1(this X2 const&);
  void fv1(this X2 const&&);
  void fv1(this X2 volatile&);
  void fv1() volatile&&;
  void fv1(this X2 const volatile&);
  void fv1(this X2 const volatile&&);

  void fcv0(this X2 &);
  void fcv0(this X2 &&);
  void fcv0(this X2 const&);
  void fcv0(this X2 const&&);
  void fcv0(this X2 volatile&);
  void fcv0(this X2 volatile&&);
  void fcv0() const volatile&;
  void fcv0(this X2 const volatile&&);

  void fcv1(this X2 &);
  void fcv1(this X2 &&);
  void fcv1(this X2 const&);
  void fcv1(this X2 const&&);
  void fcv1(this X2 volatile&);
  void fcv1(this X2 volatile&&);
  void fcv1(this X2 const volatile&);
  void fcv1() const volatile&&;
};

