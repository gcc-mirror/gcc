/* { dg-do compile } */
/* { dg-options "-O2 -march=z13 -mzarch" } */
/* { dg-final { scan-assembler-times {\tlarl\t(%r1),.LANCHOR0\n\tvllezf\t%v24,0\(\1\)\n\tbr\t%r14} 2 } } */
/* { dg-final { scan-assembler-times {\tlarl\t(%r1),.LANCHOR0\n\tvl\t%v24,4\(\1\)\n\tbr\t%r14} 1 } } */

/* Accept array arguments which are converted to pointers in the end.  */

typedef unsigned int __attribute__ ((vector_size (16))) UV4SI;
unsigned int a[42];

/* Overloaded builtin.  */

UV4SI
test_vec_insert_and_zero (void)
{
  return __builtin_s390_vec_insert_and_zero (a);
}

/* Non-overloaded builtin.  */

UV4SI
test_vllezf (void)
{
  return __builtin_s390_vllezf (a);
}

/* Directly expanded overloaded builtin.  */

UV4SI
test_vec_xl (void)
{
  return __builtin_s390_vec_xl (4, a);
}
