// PR c++/38796
// { dg-options -std=c++0x }

#define SA(X) static_assert ((X), #X)

struct A
{
  A (int);
  A (const A& = 1) = default;	// { dg-error "default argument" }
  void operator= (const A&) = default; // { dg-error "defaulted|match" }
};

struct B
{
private:
  B() = default;
};

SA(__has_trivial_constructor(B));

struct C
{
protected:
  ~C() = default;
};

SA(__has_trivial_destructor(C));

struct D
{
private:
  D& operator= (const D&) = default;
};

SA(__has_trivial_assign(D));

struct E
{
  explicit E (const E&) = default;
};

SA(__has_trivial_copy(E));

struct F
{
  F(F&) = default;		// { dg-error "non-const" }
};

struct G: public F
{
  // Can't be const because F copy ctor isn't.
  G(const G&) = default;	// { dg-error "const" }
};

struct H
{
  virtual ~H() = default;	// { dg-error "declared virtual" }
};
