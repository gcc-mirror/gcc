// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/495: Fail to locate primary class template that is
// injected by friend declaration.

template <int N> struct X
{
  template <int dim> friend struct Y;
};

X<2> x;

template <int dim> struct Y
{
  void f (Y);
  void g (Y);
};

template <int dim> void Y<dim>::f (Y)
{
}

template <int dim> void Y<dim>::g (Y<dim>)
{
}
