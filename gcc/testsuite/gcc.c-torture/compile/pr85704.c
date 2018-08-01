/* PR c/85704 */

struct C { struct {} c; };
struct D { int d; struct C e; int f; };

void
foo (struct D *x)
{
  *x = (struct D) { .e = (struct C) { .c = {} } };
}
