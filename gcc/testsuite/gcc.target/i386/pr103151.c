/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O1 -mxop" } */

typedef _Float16 v16hf __attribute__((vector_size(32)));
typedef _Float16 v8hf __attribute__((vector_size(16)));
v8hf f1() {
  int i;
  v8hf z;
  z[i] = i;
  return z;
}

v16hf f2() {
  int i;
  v16hf z;
  z[i] = i;
  return z;
}
