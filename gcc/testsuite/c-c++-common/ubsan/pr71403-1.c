/* { dg-do run } */
/* { dg-options "-O3 -fsanitize=unreachable" } */

char a = -97;
int b, c, d, e;

int
main ()
{
  int g = d, h = 0, i = 1; 
  for (; h < 3; h++)
    {
      if (g > -1)
        {
          int j;
          g = j = 0;
          for (; j < 5; j++)
          L1:
            if (!i)
              goto L1;
          a = e;
        }
      else
        i = 0;
    }
  b = c / ~(a | 114);
  __builtin_exit (0);
}
