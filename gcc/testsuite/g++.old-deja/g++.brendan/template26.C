// { dg-do assemble  }
// GROUPS passed templates
class V {
 public:
  V();
  V(int);
  };

template <int I> class AA: public virtual V {
 public:
  AA();
  AA(int);
  };

template <int I> class BB : public virtual V {
 public:
  BB();
  BB(int);
  };

template <int I> AA<I>::AA() {}
template <int I> AA<I>::AA(int i): V(i) {}
template <int I> BB<I>::BB() {}
template <int I> BB<I>::BB(int i) {}

class CC : public AA<1>, public BB<2> {
 public:
  CC();
  CC(int);
  };

