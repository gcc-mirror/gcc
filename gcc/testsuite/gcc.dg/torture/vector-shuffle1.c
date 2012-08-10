/* PR54219 */
/* { dg-do run } */

extern void abort (void);

typedef int v2si __attribute__((vector_size(2*sizeof(int))));

v2si f(v2si x)
{
  /* This requires canonicalization of the mask to { 1, 0 }.  */
  return __builtin_shuffle(x,x, (v2si) { 5, 0 });
}

int main()
{
  v2si y = f((v2si) { 1, 2 });
  if (y[0] != 2 || y[1] != 1)
    abort ();
  return 0;
}
