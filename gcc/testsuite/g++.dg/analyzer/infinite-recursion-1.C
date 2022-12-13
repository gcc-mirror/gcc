class widget
{
public:
  virtual void draw ()
  {
    /* no-op */
  }
};

class foo_widget : public widget
{
public:
  void draw ();
};

void foo_widget::draw ()
{
  // Bogus attempt to chain up to base class leading to infinite recursion:
  foo_widget::draw (); /* { dg-warning "infinite recursion" } */

  // [...snip...]
}

/* Infinite recursion due to a buggy "operator int".  */

class boxed_int
{
  int m_val;
public:
  operator int ();
};

boxed_int::operator int ()
{
  return *this; /* { dg-warning "infinite recursion" } */
}

template <typename T>
class buggy_getter
{
public:
  T get_value () const
  {
    return get_value (); /* { dg-warning "infinite recursion" } */
  }
};

int test_buggy_getter (buggy_getter<int> g)
{
  return g.get_value ();
}

/* Copy of g++.dg/warn/Winfinite-recursion.C  */

template <typename D>
struct C
{
  void foo ()
  {
    static_cast<D *>(this)->foo (); /* { dg-warning "-Wanalyzer-infinite-recursion" } */
  }
};

struct D : C<D>
{
  // this is missing:
  // void foo() {}
};

void f (D *d)
{
  d->foo ();
}


struct E : C<D>
{
  void foo() {}
};

void g (E *e)
{
  e->foo ();
}
