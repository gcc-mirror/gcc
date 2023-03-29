inline auto L2 = [] <typename T, typename U> (T, U) -> void {};
namespace B
{
  inline auto L3 = [] <typename T, typename U> (T, U) -> void {};
}

struct C
{
  int f = [] (auto){ return 1;}(&C::f);
  C ();
};

C::C ()
{
  L2 (1, 1.2f);
  B::L3 (1u, 1.2);
}

template <typename A, typename B> int foo (A&&, B&&) {return 0;}
inline int q = foo ([](){}, [](){});
