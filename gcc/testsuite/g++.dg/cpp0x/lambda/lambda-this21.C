// PR c++/66999 - 'this' captured by reference.
// { dg-do compile { target c++11 } }

struct X {
  void bar (int n)
    {
      auto l1 = [&this] { }; // { dg-error ".this. cannot be captured by reference" }
      auto l2 = [=, &this] { }; // { dg-error ".this. cannot be captured by reference" }
    }
};
