/* { dg-do compile } */
/* { dg-options "-Wall" } */

void f(){
  int i __attribute__((vector_size(2*sizeof(int)))) = { 2, 3 };
  __builtin_shuffle (i, i); /* { dg-warning "value computed is not used" } */
  ++i?1:0; /* { dg-warning "value computed is not used" } */
}
