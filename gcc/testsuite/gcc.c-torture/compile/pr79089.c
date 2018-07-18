/* PR c/79089 */

struct S { int b; };
struct T { struct S c; } a;
int d;
struct S e;

void
foo ()
{
  e = ({ d++; a.c = (struct S) {}; });
}
