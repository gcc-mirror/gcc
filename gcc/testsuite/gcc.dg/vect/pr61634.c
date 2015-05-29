/* { dg-do compile } */

int a, b, c, d;
short *e;
void fn1 (int p1[], int p2, int p3[], int p4[], int p5[], int *p6)
{
  int f;
  c = *p1;
  d = *p5;
  (void)p6;
  for (; a; a--)
    {
      f = *e >> 2;
      *e++ = f;
      b += f * f;
      f = *e >> 2;
      *e++ = f;
    }
  p4[0] = p3[0];
  for (;; p2--)
    ;
}

