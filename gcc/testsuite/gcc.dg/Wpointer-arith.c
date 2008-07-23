/* PR 35058: -Werror= works only with some warnings. */
/* { dg-do compile } */
/* { dg-options "-Werror=pointer-arith" } */
void *a;

void *test(){
  int x=5;
  if(a) a++; /* { dg-error "wrong type argument to increment" } */
  return a+x; /* { dg-error "pointer of type" } */
}
