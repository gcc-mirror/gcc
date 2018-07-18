// pr C++/80979 ICE with late discovery of using directives during ADL
// of a friend declaration.

namespace Tiny {
  class Handsome {};
  void Dahl (Handsome &, Handsome &);

  namespace Jack {
    class Vladof {
      friend void Dahl (Vladof &, Vladof &);
    };
    void Dahl (Vladof &, Vladof &);
  }

  struct BDonk {};

  namespace Tina {
    void Dahl (BDonk &, Jack::Vladof &);
  }
  using Tina::Dahl;
}

void BoomBoom (Tiny::BDonk &vault, Tiny::Jack::Vladof &hunter)
{
  Dahl (vault, hunter);
}
