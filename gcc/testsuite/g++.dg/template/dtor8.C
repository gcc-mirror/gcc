// PR c++/43648

namespace dealii
{
  namespace FEValuesViews
  {
    template <int dim, int spacedim> struct Scalar {};
  }

  template <int dim, int spacedim>
  struct X
  {
      FEValuesViews::Scalar<dim,spacedim> scalars[dim*spacedim];

      void f()
        {
          typedef dealii::FEValuesViews::Scalar<dim,spacedim> ScalarView;
          scalars[0].ScalarView::~ScalarView ();
        }
  };

  template struct X<2,2>;
}
