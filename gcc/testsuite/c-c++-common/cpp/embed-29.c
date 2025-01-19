/* PR tree-optimization/118207 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct A { unsigned char a; };
struct B { struct A b; };
static const unsigned char c[] = {
#embed __FILE__
};
struct B d;

void
foo ()
{
  const struct B *t = (const struct B *) &c;
  d.b = t->b;
}
