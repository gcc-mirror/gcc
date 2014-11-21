/* PR tree-optimization/61773 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (char **x)
{
  char *p = __builtin_malloc (64);
  char *q = __builtin_malloc (64);
  __builtin_strcat (q, "abcde");
  __builtin_strcat (p, "ab");
  p[1] = q[3];
  __builtin_strcat (p, q);
  x[0] = p;
  x[1] = q;
}
