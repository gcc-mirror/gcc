// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR68: Grammar does not allow "friend class A<int>;" 

namespace A{
  class B{};
}

namespace B{
  class A{};
  class C{
    friend class ::A::B;
  };
}


template <typename> class K;
class J {
  friend class K<int>;
};
