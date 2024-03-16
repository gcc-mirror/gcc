/* { dg-do run } */

extern void abort (void);
short a, b;
unsigned char c = 255;
unsigned cnt;
void __attribute__((noipa))
check (int x)
{
  if (x != 0)
    abort ();
  cnt++;
}
int main()
{
  int d;
  unsigned char e;
  d = 0;
  for (; a >= 0; a--) {
    int *f = &d;
    *f = c;
  }
  e = 0;
  for (; (unsigned char)(d - 255) + e <= 1; e++)
    check (b);
  if (cnt != 2)
    abort ();
  return 0;
}
