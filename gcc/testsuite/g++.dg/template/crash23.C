// PR c++/17642

template<int dim>
int f(const int* const lsh, const int* const bbox, const int* const nghostzones, int d)
{
  for (int d=0; d<dim; ++d)
    lsh[d] - (bbox[2*d+1] ? 0 : nghostzones[d]);
}

