// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

template <int rank, int dim> class Tensor;
template <int dim>
class Tensor<1,dim>
{
  public:
    explicit Tensor (const bool initialize = true);
    Tensor (const Tensor<1,dim> &);
    double values[(dim!=0) ? (dim) : 1];
};
template <int dim>
Tensor<1,dim>::Tensor (const Tensor<1,dim> &p)
{
  for (unsigned int i=0; i<dim; ++i)
    values[i] = p.values[i];
}
template <int dim>
class KellyErrorEstimator
{
    struct PerThreadData
    {
 std::vector<std::vector<std::vector<Tensor<1,dim> > > > psi;
 PerThreadData (const unsigned int n_solution_vectors,
         const unsigned int n_components,
         const unsigned int n_q_points);
    };
};
template <int dim>
KellyErrorEstimator<dim>::PerThreadData::
PerThreadData (const unsigned int n_solution_vectors,
        const unsigned int n_components,
        const unsigned int n_q_points)
{
  for (unsigned int i=0; i<n_solution_vectors; ++i)
    for (unsigned int qp=0;qp<n_q_points;++qp)
      psi[i][qp].resize(n_components);
}
template class KellyErrorEstimator<3>;
