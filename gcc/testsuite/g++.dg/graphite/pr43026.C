/* { dg-do compile } */
/* { dg-options "-O2 -fgraphite-identity" } */

template<typename Tp > class vector { };

template <int rank, int dim> class Tensor;

template <int dim> class Tensor<1,dim> {
public:
  Tensor (const Tensor<1,dim> &);
private:
  double values[(dim != 0) ? (dim) : 1];
};

template <int dim>
#ifdef NOINLINE
// declaring this noinline prevents the ICE
__attribute__ ((noinline))
#endif
Tensor<1,dim>::Tensor (const Tensor<1,dim> &p)
{
  for (unsigned int i = 0; i < dim; ++i)
    values[i] = p.values[i];
}

template <int rank, int dim>
class Tensor {
  Tensor<rank-1,dim> subtensor[dim];
};

template <int dim> class Base {
public:
  const unsigned int npoints;
  const unsigned int dofs;
  const Tensor<2,dim> &s2d (const unsigned int fno,
                            const unsigned int pno) const;
  void getf2d (vector<Tensor<2,dim> >& d2) const;
};

template <int dim>
void Base<dim>:: getf2d
  (vector<Tensor<2,dim> > &d2) const
{
  unsigned int point, sf;

  for (point = 0; point < npoints; ++point)
    for (sf = 0; sf < dofs; ++sf)
      Tensor<2,dim> tmp = s2d (sf, point);
}

template void Base<3>::getf2d (vector<Tensor<2,3> > &) const;
