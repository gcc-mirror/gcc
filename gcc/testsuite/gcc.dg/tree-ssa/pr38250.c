/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

__extension__ typedef __SIZE_TYPE__ size_t;
typedef struct   {
          long dat[2];
} gsl_complex_long_double;
typedef struct {
 size_t size;
 size_t stride;
 long *data;
} gsl_vector_complex_long_double;

void gsl_vector_complex_long_double_set_zero (gsl_vector_complex_long_double * v) 
{
 long * const data = v->data;
 const size_t n = v->size;
 const size_t stride = v->stride;
 const gsl_complex_long_double zero = { { 0,0} } ;
 size_t i;
 for (i = 0; i < n; i++)     
  *(gsl_complex_long_double *) (data + 2 * i * stride) = zero;
}

