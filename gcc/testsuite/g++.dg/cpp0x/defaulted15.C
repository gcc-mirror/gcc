// PR c++/38796
// { dg-options -std=c++0x }

struct A
{
  A (int);
  A (const A& = 1) = default;	// { dg-error "default argument" }
  void operator= (const A&) = default; // { dg-error "defaulted|match" }
};

struct B
{
private:
  B() = default;		// { dg-error "access" }
};

struct C
{
protected:
  ~C() = default;		// { dg-error "access" }
};

struct D
{
private:
  D& operator= (const D&) = default; // { dg-error "access" }
};

struct E
{
  explicit E (const E&) = default; // { dg-error "explicit" }
};

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
