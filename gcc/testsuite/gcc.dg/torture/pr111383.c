/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);
int a, d = 1625015426;
char e;
short b;
short *f = &b, *g = &b;
void h(char *k) {
  char c = *k;
  for (; c;)
    ;
}
int main()
{
  *g = 25330;
  int i, j;
  i = 0;
  for (; *f + d - 1625040257 < 7;)
    ;
  for (; i < 4; i++) {
    j = 0;
    for (; (d - 1625015511) + (d - 1625015341) + j < 1; j++)
      h(&e);
  }
  if (a != 0)
    abort ();
  return 0;
}
