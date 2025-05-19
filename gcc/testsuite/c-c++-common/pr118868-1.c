/* { dg-do compile } */

/* PR middle-end/118868 */

/* __builtin_assoc_barrier should work on pointers without any ICE */
void *f(void *a)
{
  return __builtin_assoc_barrier(a);
}
