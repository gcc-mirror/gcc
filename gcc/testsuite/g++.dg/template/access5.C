// { dg-do compile }
// Origin: Wolfgang Bangerth <wolfgang.bangerth@iwr.uni-heidelberg.de>

// PR c++/7348
// Access control for typename in function return type

class Outer {
    template <int dim> struct Inner {
        typedef int T;
        T foo ();
    };
  public:
    Outer();
};

template <int dim>
typename Outer::Inner<dim>::T  Outer::Inner<dim>::foo () {
  return 1;
}

template struct Outer::Inner<2>;
