/* { dg-do compile } */
/* { dg-options "-O -w" } */
int a = 1, g, h = 1, d, e, *f;
char b;
static int c[] = { 0, 0 };
void fn2 (void);

void
fn1 (short x, int l)
{
lab:
  {
    int k, m[0];
    long j = h ? 0 : 0 / 0;
    unsigned char n = j;
    unsigned char i = x >= 0 ? n : n >> x;
    g = i;
    for (;;)
      {
        if (a)
          goto lab;
        while (d)
          {
            e = b = c[l];
            fn2 ();
          }
        int o = m[0];
        f = &k;
      }
  }
}
