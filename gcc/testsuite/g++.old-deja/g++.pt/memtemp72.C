// Build don't link:

template<class P> struct B
{
  template<class T> void f(T& t) { t = T(); }
};

enum ptype { t1, t2};

struct D : public B<ptype>
{
  void g(double& d) { f(d); }
};


D d;
 
