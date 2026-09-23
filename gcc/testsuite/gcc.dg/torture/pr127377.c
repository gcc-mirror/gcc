/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

__attribute__((noipa))
int q(void) {
  struct h {
    unsigned i : 1;
    int : 1;
    unsigned j : 3;
    int k : 7;
    int : 3;
    int aa : 6;
    unsigned l : 7;
  };
  union {
    int m;
    signed char b[4];
    struct h ab;
  } n = {};
  int o;
  n.m = 0x550f;
  for (int i = 0; i < 1; i++) 
  {
    n.b[2] = n.ab.i + n.ab.aa;
    n.m = n.m & ~28u | ((0x7 ^ n.ab.aa) & 7) << 2;
  }
  n.b[3] = 0;
  n.m = n.m | 0x08000000;
  n.b[2] = 0x7;
  o = n.ab.l;
  return o;
}

int main()
{
  int t1 =  q();
  if (t1 != 64)
    __builtin_abort ();
  return 0;
}
