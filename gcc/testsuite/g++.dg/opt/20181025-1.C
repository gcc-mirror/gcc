// { dg-do compile }
// { dg-options "-Ofast" }

template <typename Number>
class Vector {
    typedef Number value_type;
    typedef const value_type *const_iterator;
    Number norm_sqr () const;
    const_iterator begin () const;
    unsigned int dim;
};
template <typename Number>
static inline Number
local_sqr (const Number x)
{
  return x*x;
}
template <typename Number>
Number
Vector<Number>::norm_sqr () const
{
  Number sum0 = 0, sum1 = 0, sum2 = 0, sum3 = 0;
  const_iterator ptr = begin(), eptr = ptr + (dim/4)*4;
  while (ptr!=eptr) 
    {
      sum0 += ::local_sqr(*ptr++);
      sum1 += ::local_sqr(*ptr++);
    }
  return sum0+sum1+sum2+sum3;
}
template class Vector<double>;
