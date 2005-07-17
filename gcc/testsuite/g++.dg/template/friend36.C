// PR c++/22139
// { dg-options "--param ggc-min-expand=0 --param ggc-min-heapsize=0" }
 
template <int rank, int dim> class Tensor;
template <int rank, int dim> struct SymmetricTensor {     
  SymmetricTensor (const Tensor<2,dim> &t);
  friend void foo(); 
};
template <> SymmetricTensor<2,2>::SymmetricTensor (const Tensor<2,2> &t) {}
template <> SymmetricTensor<2,3>::SymmetricTensor (const Tensor<2,3> &t) {}
