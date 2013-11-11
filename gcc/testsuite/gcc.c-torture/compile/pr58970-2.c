/* PR middle-end/58970 */

struct T { char a : 8; char b : 1; };
struct S { char x; struct T t[1]; };

void
foo (int x, struct S *s)
{
  if (x == -1)
    s->t[x].b = 0;
}
