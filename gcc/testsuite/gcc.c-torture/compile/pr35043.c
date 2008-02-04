typedef long unsigned int size_t;
typedef struct   {
      long double dat[2];
} gsl_complex_long_double;
typedef struct {
    size_t size;
    size_t stride;
    long double *data;
} gsl_vector_complex_long_double;
void gsl_vector_complex_long_double_set_zero (gsl_vector_complex_long_double * v) 
{
    long double * const data = v->data;
    const size_t n = v->size;
    const size_t stride = v->stride;
    const gsl_complex_long_double zero = { { 0.0L,0.0L} } ;
    size_t i;
    for (i = 0; i < n; i++)     
        *(gsl_complex_long_double *) (data + 2 * i * stride) = zero;
}

