/* PR tree-optimization/34648 */

/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions" } */

extern const unsigned short int **bar (void) __attribute__ ((const));
const char *a;
int b;
char c;

char
foo (int *x)
{
  char r;

  c = '\0';
  if (!b)
    {
      while (((*bar ())[a[*x]] & 0x2000) != 0)
        (*x)++;
      if (a[++(*x)] == '-')
        {
          (*x)++;
          if (a[*x] && !((*bar ())[a[*x]] & 0x2000))
            return '?';
        }
      if (!a[*x] || ((*bar ())[a[*x]] & 0x2000))
        {
          while (((*bar ())[a[*x]] & 0x2000))
            ++(*x);
          return '\0';
        }
    }

  r = a[*x];
  b = a[*x] && !((*bar ())[a[*x]] & 0x2000);
  return r;
}

