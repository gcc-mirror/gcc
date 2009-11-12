// PR c++/39560
// { dg-options -Wunused }

struct X { };

class Z {
public:
  X* cc(int c);
};

class F {
public:
  typedef X* (Z::*MethO)(int);
  typedef X* (F::*MethF)(int);
  template<MethO m>
  X* xwrapper(int i) {
    union {
      Z *z;
      F *f;
    };				// { dg-bogus "unused" }
    f = this;
    return ((z->*m)(i));
  }
};

F::MethF meth = &F::xwrapper<&Z::cc>;
