/* { dg-do compile } */
/* { dg-options "-mavx2 -O" } */

typedef char v16qi __attribute__((vector_size(16)));

v16qi foo(v16qi a){
  return __builtin_ia32_vec_set_v16qi (a, -1, 2);
}
