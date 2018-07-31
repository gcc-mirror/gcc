// Core Issue #1331 (const mismatch with defaulted copy constructor)
// { dg-do compile { target c++11 } }

struct M
{
  M(M&);
};

struct W
{
  W();
  W(const W&);
  M m;
};

// Not first declaration.
W::W(const W&) = default; // { dg-error "binding" }
W w;
