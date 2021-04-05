// PR c++/90664
// { dg-do compile { target c++11 } }

template <typename TT, typename MFP, MFP> struct OpM;

template <typename TR, typename TT, TR (TT::*f)()>
struct OpM<TT, TR (TT::*)(), f>
{};

class Class {
public:
  int address() noexcept { return 0; }
  void address(int) noexcept {}
};

struct Sk {
  template <class C, typename R> Sk(R (C::*p)()) {
    typedef OpM<C, R (C::*)() /* noexcept */, &Class::address> OP;
  }
};

Sk sk(static_cast<int (Class::*)()>(&Class::address));
