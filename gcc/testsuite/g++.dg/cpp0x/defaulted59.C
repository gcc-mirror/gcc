// Core Issue #1331 (const mismatch with defaulted copy constructor)
// { dg-do compile { target c++11 } }

struct M
{
  M(M&) = default;
};

struct W : public M
{
  W(const W&) = default;
};
