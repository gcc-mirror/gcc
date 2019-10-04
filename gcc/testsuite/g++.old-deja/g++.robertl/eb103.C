// { dg-do assemble  }
// Error: Internal compiler error in 2.7.2 & EGCS 1.0.0

template <int nlimb, int i>
inline unsigned f (unsigned* ptr);
template <int nlimb>
inline unsigned f<nlimb,nlimb> (unsigned* ptr)  // { dg-error "17:non-class, non-variable partial specialization" "" { target c++14 } }
// { dg-error "17:non-type partial specialization" "" { target c++11_down } .-1 }
{
  return 1;
}

