// PR c++/24569

template <int dim>
struct S
{
  static const int u = 2 * dim;
  static const int p[u];
  static int f();
};

template <>
inline int S<3>::f () { return 1; }

template <int dim> const int S<dim>::u;

template class S<3>;
