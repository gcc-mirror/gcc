/* PR middle-end/66820 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void bar (char *);

void
foo (char **x)
{
#pragma omp parallel for
  for (int i = 0; i < 16; i++)
    {
      char y[50];
      __builtin_strcpy (y, x[i]);
      __builtin_strcat (y, "foo");
      bar (y);
    }
}
