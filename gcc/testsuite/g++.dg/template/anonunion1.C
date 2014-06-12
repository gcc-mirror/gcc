// PR c++/47303
// { dg-do compile }


struct Z
{
  void foo (int);
};

struct F
{
  typedef void (Z::*zm) (int);
  typedef void (F::*fm) (int);
  template <zm>
  void bar (int)
  {
    union
    {
      Z z;
    };
  }
};

F::fm m = &F::bar <&Z::foo>;
