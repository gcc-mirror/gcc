// { dg-do assemble  }

template <unsigned rank>
class Tensor
{
};

template <unsigned rank>
class Tensor<2> : Tensor<rank> { // { dg-error "" } template parameters not used
};

