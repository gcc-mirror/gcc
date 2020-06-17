// PR c++/93905
// { dg-do compile { target c++11 } }

enum class E { VALUE };

struct B {
  E e{E::VALUE};
protected:
  ~B () = default;  
};

struct D : B {};

int
main ()
{
  D d{};
}
