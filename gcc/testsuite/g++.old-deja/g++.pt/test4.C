// { dg-do assemble  }

class B { };
template <class x, int b> class X : public B { int y[b]; };
