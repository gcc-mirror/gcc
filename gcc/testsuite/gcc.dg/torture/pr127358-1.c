/* { dg-do run } */
/* PR tree-optimization/127358 */

__attribute__((noipa))
int compute(unsigned x){
  unsigned max = __INT_MAX__;
  if (x != __INT_MAX__
      && x != (((unsigned)__INT_MAX__)+1))
    __builtin_unreachable();
  int r = (x == max) ? 5 : 6;
  return r;
}

int main(void){
  volatile unsigned a = __INT_MAX__;
  volatile unsigned b = a+1;
  if (compute(a) != 5)
    __builtin_abort ();
  if (compute(b) != 6)
    __builtin_abort ();
  return 0;
}

