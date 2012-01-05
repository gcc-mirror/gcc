/* PR middle-end/51761 */

struct S { unsigned int len; };
struct S foo (struct S);

struct S
bar (struct S x)
{
  return ({ struct S a = x; foo (a); });
}
