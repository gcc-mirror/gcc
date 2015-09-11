// PR c++/58923

struct A
{
  virtual void do_it() const = 0;
};

struct B :  A
{
  virtual void do_it() const {}
};

struct C
{
  operator B() const { return B(); }
};

void do_it(const A& a) { a.do_it(); }

int main()
{
  C c;
  do_it(c);
  return 0;
}
