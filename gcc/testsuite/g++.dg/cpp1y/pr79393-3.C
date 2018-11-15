// pr c++/79393
// { dg-do compile { target c++14 } }

struct A 
{
  friend class C;
private:
  ~A () noexcept (false);
};

A::~A () noexcept(false) {}

struct B : virtual A
{
  // non-virtual, abstract, ignores A
  ~B ();
  virtual void abs () = 0;
};

B::~B () {
  throw 1; // { dg-warning "will always call terminate" }
}

struct C : virtual A
{
  // non-virtual, non-abstract, considers A
  ~C ();
  virtual void abs ();
};

C::~C () {
  throw 1;
}

struct D : virtual A
{
  // virtual, abstract, considers A
  virtual ~D ();
  virtual void abs () = 0;
};

D::~D () {
  throw 1;
}
