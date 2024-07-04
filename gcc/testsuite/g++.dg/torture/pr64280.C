// { dg-do compile }

class A
{
public:
  A ();
};
class B
{
public:
  B (int);
  operator void *() { return m_fn1 () ? 0 : this; }
  int m_fn1 ();
};
typedef int jmp_buf[];
struct C
{
  jmp_buf cond_;
};
class F
{
  C what_;
  bool m_fn2 ();
};
int _setjmp (int[]);
void longjmp ();
class D
{
public:
  D () { longjmp (); }
};
bool
F::m_fn2 ()
{
  B a (0);
  if (a)
    if (_setjmp (what_.cond_))
      return 0;
    else
      D ();
  A b;
}	// { dg-warning "control reaches end of non-void function" }
