// PR c++/51620

template<int> class A
{
  virtual ~A();			// { dg-error "non-deleted|private" }
};

struct B : A<0>, A<1>		// { dg-error "deleted|context" }
{
  B() {}			// { dg-error "context" }
};
