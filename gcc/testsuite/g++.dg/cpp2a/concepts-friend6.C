// PR c++/93467
// { dg-do compile { target c++20 } }

template<bool B> requires B
  class C;

template<typename>
class S1
{
  template<bool B> requires B
    friend class ::C;
};

template<typename>
class S2
{
  template<bool B> requires (!B)
    friend class ::C; // { dg-error "does not match original declaration" }
};
