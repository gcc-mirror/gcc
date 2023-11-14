/* { dg-do run } */
/* { dg-additional-options "-fsplit-loops" } */

int a, c, f;
char b, g;
int *d = &c;
long e;
int main()
{
  for (; e != 25; e++) {
    f = -17;
    for (; f <= 0; f = f + 7) {
      g = f ? 0 : b;
      a = *d;
    }
  }
  if (a != 0)
    __builtin_abort ();
}
