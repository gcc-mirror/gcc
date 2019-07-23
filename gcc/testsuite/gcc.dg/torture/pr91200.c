/* { dg-do compile } */

int printf (const char *, ...);

char a;
int b, c, **d;

int main ()
{
  int f = -128, *g, *h[2] = {0, 0}, i;
  printf("0");
  if (a)
    {
      while (f > a) {
        int *j = &i;
        *j |= 0;
      }
      h[i] = &c;
    }
  if (h[1])
    {
      int **k = &g;
      *k = &f;
      while (i)
        {
          int **l[] = {&g};
        }
      int **m = &g;
      *d = *m = &b;
    }
  return 0;
}
