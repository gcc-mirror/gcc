// PR c++/34488
// { dg-do compile }

struct A
{
  A ();
  ~A ();
  A (const A &);
};

struct B
{
  friend A::A ();
  friend A::~A ();
  friend A::A (const A &);
};

struct C
{
  friend int C ();		// { dg-error "return type|in friend decl" }
  friend int ~C ();		// { dg-error "return type|in friend decl" }
  friend int C (const C &);	// { dg-error "return type|in friend decl" }
};

struct D
{
  friend int D () {}		// { dg-error "return type|in friend decl" }
  friend int ~D () {}		// { dg-error "return type|in friend decl" }
  friend int D (const D &) {}	// { dg-error "return type|in friend decl" }
};

struct E
{
  friend A::A () {}		// { dg-error "cannot define member" }
  friend A::~A () {}		// { dg-error "cannot define member" }
  friend A::A (const A &) {}	// { dg-error "cannot define member" }
};
