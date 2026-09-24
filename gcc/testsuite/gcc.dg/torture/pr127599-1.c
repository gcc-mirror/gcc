/* { dg-do compile } */
/* PR target/127599 */

#define vector(s,t) t __attribute__((vector_size(s*sizeof(t))))
vector(4,int) b1;
void c1() {
  b1 = __builtin_shufflevector (b1, b1, 1,0,3,2);
  b1 = __builtin_shufflevector (b1, b1, 2,3,4,5);
}
