// { dg-do compile }
// { dg-options "-fopenmp" }
// { dg-require-effective-target fopenmp }

class GException {
public:
    class vector_mismatch {
    public:
	vector_mismatch(int size1, int size2);
    };
};
class GVector{
public:
    GVector& operator+=(const GVector& v);
    int m_num;
    double* m_data;
};
inline GVector& GVector::operator+= (const GVector& v)
{
  if (m_num != v.m_num)
    throw GException::vector_mismatch(m_num, v.m_num);
  for (int i = 0; i < m_num; ++i)  m_data[i] += v.m_data[i];
};
void eval(GVector* m_gradient, GVector* vect_cpy_grad, int n)
{
#pragma omp sections
    {
      for (int i = 0; i < n; ++i)
	*m_gradient += vect_cpy_grad[i];
    }
}
