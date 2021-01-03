/* PR c/98087 */

struct S { char a; long long b; };
struct T { struct S c[0]; char d; };
void foo (int n)
{
  struct S a[n][0];
  __builtin_clear_padding (a);
  __builtin_clear_padding (&a);
  struct S b[7][0];
  __builtin_clear_padding (&b);
  struct T c;
  __builtin_clear_padding (&c);
}
