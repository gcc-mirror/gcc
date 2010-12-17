/* { dg-lto-do assemble } */

static inline int __gthread_active_p (void) { }
template <int rank, int dim> class Tensor;
template <int dimension> struct G;
template <int dim> class T {
    typedef void A;
    typedef Tensor<1,dim> F[G<dim>::v];
};
