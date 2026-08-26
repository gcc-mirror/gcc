// Check whether deferred deduction works correctly when post conditions
// are used in member functions.
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }

bool check (bool b) { return b; }

class S
{
  bool f ()
    post (r: check (r))
    post (r: r)
  {}
};
