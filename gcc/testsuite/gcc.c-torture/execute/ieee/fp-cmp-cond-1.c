/* PR tree-optimization/111109 */

/*
   f should return 0 if either fa and fb are a nan.
   Rather than the value of a or b.
*/
__attribute__((noipa))
int f(int a, int b, float fa, float fb) {
  const _Bool c = fa < fb;
  const _Bool c1 = fa >= fb;
  return (c * a) | (c1 * b);
}

/*
   f1 should return 0 if either fa and fb are a nan.
   Rather than the value of a&1 or b&1.
*/
__attribute__((noipa))
int f1(int a, int b, float fa, float fb) {
  const _Bool c = fa < fb;
  const _Bool c1 = fa >= fb;
  return (c & a) | (c1 & b);
}

#if __SIZEOF_INT__ == __SIZEOF_FLOAT__
typedef int v4si __attribute__ ((vector_size (1*sizeof(int))));
typedef float v4sf __attribute__ ((vector_size (1*sizeof(float))));
/*
   fvf0 should return {0} if either fa and fb are a nan.
   Rather than the value of a or b.
*/
__attribute__((noipa))
v4si vf0(v4si a, v4si b, v4sf fa, v4sf fb) {
  const v4si c = fa < fb;
  const v4si c1 = fa >= fb;
  return (c & a) | (c1 & b);
}


#endif

int main(void)
{
  float a = __builtin_nan("");

  if (f(-1,-1, a, a) != 0)
    __builtin_abort();
  if (f(-1,-1, a, 0) != 0)
    __builtin_abort();
  if (f(-1,-1, 0, a) != 0)
    __builtin_abort();
  if (f(-1,-1, 0, 0) != -1)
    __builtin_abort();


  if (f1(1,1, a, a) != 0)
    __builtin_abort();
  if (f1(1,1, a, 0) != 0)
    __builtin_abort();
  if (f1(1,1, 0, a) != 0)
    __builtin_abort();
  if (f1(1,1, 0, 0) != 1)
        __builtin_abort();

#if __SIZEOF_INT__ == __SIZEOF_FLOAT__
  v4si b = {-1};
  v4sf c = {a};
  v4sf d = {0.0};
  if (vf0(b,b, c, c)[0] != 0)
    __builtin_abort();
  if (vf0(b,b, c, d)[0] != 0)
    __builtin_abort();
  if (vf0(b,b, d, c)[0] != 0)
    __builtin_abort();
  if (vf0(b,b, d, d)[0] != b[0])
        __builtin_abort();
#endif
}
