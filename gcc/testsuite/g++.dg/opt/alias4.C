// PR c++/27768
// Alias grouping was losing some may_aliases, causing us to think
// the store to w.p was dead.

// { dg-do run }
// { dg-options "-O2" }

int N = 1;

struct VA
{
  int *p, *q, *r;

  VA() : p(), q() {}
  VA(const VA&) : p(), q() {}
  ~VA() { if (p) --N; }
};

inline void foo(VA, VA, VA) {}

struct VB
{
  VA va;

  VB() {}

  VB(const VB&)
  {
    va.p = new int(va.q - va.p);
    va.r = va.p + (va.q - va.p);
    foo(va, va, va);
  }
};

struct VC : VB { char c; };
struct V : VC {};

struct WA
{
  struct X {};
  X **p, **q, **r;

  WA() : p() {}
  ~WA() { if (p) --N; }
};

struct W : WA {};

int main()
{
  {
    V v, u(v);
    W w;
  }
  return N;
}
