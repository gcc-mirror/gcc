// Test that converting a COND_EXPR to void doesn't result in trying to
// bitwise copy a class with a nontrivial copy constructor (and thus a
// compiler abort).

// { dg-options "-O" }

struct A {
  virtual ~A() { }
};

A a1, a2;
inline A& one () { return a1; }
inline A& two () { return a2; }

inline void f (int i)
{
  i ? a1 : a2;
  i ? one() : two();
}

int main ()
{
  f (1);
}
