/* { dg-do run } */
/* { dg-additional-options "-fno-tree-forwprop" } */
/* PR tree-optimization/127689 */


__attribute__((noipa))void check(int v)
{
    if (v != -4)
      __builtin_exit(1);
}
int p;
signed char s, v;
signed char t = 0x80;
__attribute__((noipa))
void y(signed char w) {
  int t1 = w;
  t1 = __builtin_abs(t1); //256, 0x80
  p = t1;
  s = p;
  signed char t2 = t1; // -256, 0x80
  t = t2 % 3; // -2
  v = 2 * t; // -4
    check(v);
}
int main()
{
    y(0x80);
}
