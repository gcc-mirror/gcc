// Build don't link:

template <unsigned rank>
class Tensor
{
};

template <unsigned rank>
class Tensor<2> : Tensor<rank> { // ERROR - template parameters not used
};

