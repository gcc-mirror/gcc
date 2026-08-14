// PR c++/125011
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fcontracts" }

template<bool x>
  bool f()
    post(r: r == x)
  {
    return x;
  }

template bool f<true> ();
