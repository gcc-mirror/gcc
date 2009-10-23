// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/40808
// { dg-do compile }
// This tests the mangling of empty template argument list in a template
// id.
// { dg-final { scan-assembler "_ZNK5DummyclI3GenEENT_3SigIE10ResultTypeERKS2_" } }


struct Void {};

template <class R> struct FunType {
  typedef R ResultType;
};

struct WrongNumberOfSigArgs {};

template <typename R> struct CFunType {
  template <class Dummy1=Void, class Dummy2=Void> struct Sig : public
FunType<WrongNumberOfSigArgs> {};
  template <class Dummy> struct Sig<Void,Dummy> : public FunType<R> {};
};

struct Dummy {
  template <typename F> typename F::template Sig<>::ResultType operator()(F
const& f) const {
    return typename F::template Sig<>::ResultType(0);
  }
};

struct Gen: public CFunType<int> {
  int operator()() const {return 0;}
  Gen() {}
};

int myfunction() {
  return Dummy()(Gen());
}

int main() {
  myfunction();
}
