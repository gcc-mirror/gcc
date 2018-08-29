// { dg-options "-O" }

struct Outer {
  struct Inner { virtual bool f() = 0; };
  void g(Inner &) const;
};
 
inline void h(const Outer &o)
{
  struct Local : public Outer::Inner {
    virtual bool f() { return true; };
  };
  Local l;
  o.g(l);
}
 
void f(Outer &req) {
  h (req);
}
 
