// Build don't link:
// Special g++ Options: -funroll-loops -O2 -g

inline void f()
{
  typedef int T;
}

inline void g()
{
  typedef double U;
}

int n;

struct B
{
  ~B() { 
    for (int i = 0; i < n; ++i)
      g(); 
  }
};

struct D : public B {
  ~D() { 
    for (int j = 0; j < n; ++j)
      f(); 
  }
};

D d;
