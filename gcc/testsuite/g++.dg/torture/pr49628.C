/* { dg-do compile } */

#include <vector>

template <int rank, int dim> class Tensor;
template <int dim> class Tensor<1,dim> {
public:
    explicit Tensor (const bool initialize = true);
    Tensor (const Tensor<1,dim> &);
    Tensor<1,dim> & operator = (const Tensor<1,dim> &);
    double values[(dim!=0) ? (dim) : 1];
};
template <int dim>
inline Tensor<1,dim> & Tensor<1,dim>::operator = (const Tensor<1,dim> &p)
{
  for (unsigned int i=0; i<dim; ++i)
    values[i] = p.values[i];
};
template <int dim> class Quadrature {
public:
    const unsigned int n_quadrature_points;
};
class MappingQ1
{
  class InternalData  {
  public:
      std::vector<Tensor<1,3> > shape_derivatives;
      unsigned int n_shape_functions;
  };
  void compute_data (const Quadrature<3> &quadrature, InternalData &data)
      const;
};
void MappingQ1::compute_data (const Quadrature<3> &q, InternalData &data) const
{
  const unsigned int n_q_points = q.n_quadrature_points;
  data.shape_derivatives.resize(data.n_shape_functions * n_q_points);
}
