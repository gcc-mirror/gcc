// PR c++/119383
// { dg-do run { target c++11 } }

int g;

struct base {
  virtual base *clone() const = 0;
  ~base() { }
};

struct impl : virtual base {
  base *clone() const { return new impl; }  // #1
  impl() { ++g; }
  ~impl() { --g; }
};

const base *
make_a_clone ()
{
  const base &base = impl{}; // #2
  return base.clone();
}

int
main ()
{
  make_a_clone ();
  // impl::impl() is called twice (#1 and #2), impl::~impl() once,
  // at the end of make_a_clone.
  if (g != 1)
    __builtin_abort ();
}
