// { dg-do compile }
// Origin: Wolfgang Bangerth <wolfgang.bangerth@iwr.uni-heidelberg.de>

// PR c++/7347
// Access control for typename during instantiation

template <int dim> class Base {
  protected:
    typedef int T;
};

template <int dim> class D : public Base<dim> {
  public:
    typedef typename Base<dim>::T T1;
    D (T1 t);
};

D<2> d(1);
