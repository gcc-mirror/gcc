/* { dg-do compile } */
/* PR tree-optimization/123382 */

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))

typedef BS_VEC(short, 4) v4s;
void f(int l, v4s *a, bool *b)
{
  for(int i =0;i < l; i++)
  {
    v4s t = a[i];
    if (b[i])
      t = __builtin_shufflevector(t, t, 3,3,2,3);
    else
      t = __builtin_shufflevector(t, t, 0,0,2,3);
    a[i] = t;
  }
}
