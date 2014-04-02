// { dg-do compile { target c++11 } }

struct Iter
{
  int& operator* ();
  void operator++ ();
};

bool operator!= (Iter &, Iter &) { }

struct Container
{
  Iter begin () const;
  Iter end () const;
};

struct J
{
  virtual J *mutable_child ();
};

struct M
{
  M (const Container &);
  J ns_;
};
namespace
{
  J MakeNamespace (const Container &src)
    {
      J a;
      J *b = 0;
      for (const int &c: src)
	b = b ? b->mutable_child () : &a;
      return a;
    }
}
M::M (const Container &ns):ns_ (MakeNamespace (ns))
{
}
