// { dg-do assemble  }

struct U {
  static int STATIC;
};

template <int* x> class FOO {
public:
  enum { n = 0 };
};

template <class A> class BAR {
public:
  enum { n = FOO<&A::STATIC>::n };
};

int n = BAR<U>::n;
