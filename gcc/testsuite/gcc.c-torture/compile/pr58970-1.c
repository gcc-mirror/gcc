/* PR middle-end/58970 */

struct T { int b : 1; };
struct S { struct T t[1]; };

void
foo (int x, struct S *s)
{
  if (x == -1)
    s->t[x].b = 0;
}
