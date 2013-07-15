/* { dg-do compile } */
/* { dg-options "-std=gnu++1y -Wsign-conversion" } */

typedef double vecd __attribute__((vector_size(4*sizeof(double))));
typedef float vecf __attribute__((vector_size(8*sizeof(float))));
typedef long vecl __attribute__((vector_size(4*sizeof(long))));
typedef short vecs __attribute__((vector_size(8*sizeof(short))));
typedef char vecc __attribute__((vector_size(16*sizeof(char))));

auto f(vecf*a,float d,long long i){
  return (*a<0)?d:i; // { dg-error "truncation" }
}
auto g(vecc*a){
  return (*a<0)?3LL:42UL; // { dg-error "inferred scalar type" }
}
auto h(vecd*a){
  return (*a<0)?'a':'c'; // { dg-error "inferred scalar type \[^\\n\]*double" }
}
auto i(vecc*a){
  return (*a<0)?1:0.; // { dg-error "inferred scalar type" }
}
auto j(vecl*a,long i,unsigned long k){
  return (*a<0)?i:k; // { dg-warning "may change the sign" }
}
