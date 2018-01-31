/* { dg-do compile } */
/* { dg-options "-fno-early-inlining"  } */

template <int dim> class B;
template <int, int dim> class TriaObjectAccessor;
template <int, typename Accessor> class A;
template <int dim> class TriaDimensionInfo {
public:
  typedef A<3, TriaObjectAccessor<2, 3> > raw_quad_iterator;
  typedef A<3, B<3> > raw_hex_iterator;
  typedef raw_hex_iterator raw_cell_iterator;
};
template <int dim> class Triangulation : public TriaDimensionInfo<1> {
  public:
  typedef typename TriaDimensionInfo<dim>::raw_quad_iterator raw_quad_iterator;
  TriaDimensionInfo::raw_cell_iterator end() const;
  raw_quad_iterator end_quad() const {
    return raw_quad_iterator(const_cast<Triangulation *>(this), 0, 0);
  }
};
template <int dim> class TriaAccessor {
public:
  typedef void AccessorData;
  TriaAccessor(const Triangulation<dim> * = 0);
  Triangulation<1> *tria;

  int a, b, c;
};
template <int dim> class TriaObjectAccessor<2, dim> : public TriaAccessor<dim> {
public:
  typedef typename TriaAccessor<dim>::AccessorData AccessorData;
  TriaObjectAccessor(const Triangulation<dim> * = 0);
};
template <int dim> class TriaObjectAccessor<3, dim> : public TriaAccessor<dim> {
public:
  typedef typename TriaAccessor<dim>::AccessorData AccessorData;
  TriaObjectAccessor(const Triangulation<dim> * = 0);
};
template <int dim> class B : public TriaObjectAccessor<dim, dim> {
public:
  typedef typename TriaObjectAccessor<dim, dim>::AccessorData AccessorData;
  B(const Triangulation<dim> * = 0);
};
template <int dim, typename Accessor> class A {
public:
  A(const A &);
  A(const Triangulation<dim> *, int, int);
  Accessor accessor;
};
template class Triangulation<3>;
template <int dim, typename Accessor>
A<dim, Accessor>::A(const Triangulation<dim> *, int, int) {}
template <int dim>
TriaAccessor<dim>::TriaAccessor(const Triangulation<dim> *)
    : tria(), a(-1), b(-2), c(-3) {}
template <int dim>
TriaObjectAccessor<2, dim>::TriaObjectAccessor(const Triangulation<dim> *) {}
template <int dim>
TriaObjectAccessor<3, dim>::TriaObjectAccessor(const Triangulation<dim> *) {}
template <int dim> B<dim>::B(const Triangulation<dim> *) {}
template <>
TriaDimensionInfo<3>::raw_cell_iterator Triangulation<3>::end() const {
  return raw_hex_iterator(const_cast<Triangulation *>(this), 0, 0);
}

#pragma GCC optimize ("-O0")
int main()
{
  Triangulation <3> t;
  Triangulation<3>::raw_quad_iterator i1 = t.end_quad();
  TriaDimensionInfo<3>::raw_cell_iterator i2 = t.end();

  if(i2.accessor.c != -3)
    return 1;

  return 0;
}

