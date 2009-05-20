/* PR middle-end/40204 */

struct S
{
  unsigned int a : 4;
  unsigned int b : 28;
} s;
char c;

void
f (void)
{
  s.a = (c >> 4) & ~(1 << 4);
}
