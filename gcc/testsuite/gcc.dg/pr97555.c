// { dg-do run }
// { dg-options "-Os" }

struct {
  int a:1;
} b;

int c, d, e, f = 1, g;

int main ()
{
  for (; d < 3; d++) {
    char h = 1 % f, i = ~(0 || ~0);
    c = h;
    f = ~b.a;
    ~b.a | 1 ^ ~i && g;
    if (~e)
      i = b.a;
    b.a = i;
  }
  return 0;
}
