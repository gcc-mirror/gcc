// { dg-do assemble  }
// GROUPS passed templates membertemplates
template <class T>
struct S
{
  S(const S& s) {}

  template <class U>
  S(S<U>& s)
  {
    S<U> s2(s);
  }
};


extern S<int>& si;

void foo()
{
  S<char*> sc(si);
}
