/* PR tree-optimization/19899 */
/* Decrementing a floating-point variable in a loop caused an ICE.  */

/* { dg-do run } */
/* { dg-options "-O -ftree-vectorize" } */

extern void abort (void);

int main()
{
  float i=1;

  while (i>=0)
    --i;

  if (i != -1)
    abort();
  return 0;
}
