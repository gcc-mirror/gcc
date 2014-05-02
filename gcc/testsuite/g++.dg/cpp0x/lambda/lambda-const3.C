// PR c++/60992
// { dg-do compile { target c++11 } }

struct ScopeGuardGenerator { };

struct FF
{
  template < class F, class ... Ts >
  void
  operator () (F & ...)
  {
    const int n = sizeof ... (Ts) + 1;
    void *mutexes[n];
    auto _on_scope_exit_var_0 =
      ScopeGuardGenerator () + [&mutexes] { };
  }
};

template < class F >
int operator+ (ScopeGuardGenerator, F) { return 1; }

struct D
{
  template < class T0, class T1, class T2, class ... T >
  void
  operator () (T0, T1, const T2 & t2, T & ... t)
  {
    base (t2, t ...);
  }
  FF base;
};

D run_with_locks;

void Fn ()
{
  run_with_locks ([] { }, 0, 0);
}
