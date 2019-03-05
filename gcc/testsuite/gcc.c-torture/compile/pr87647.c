/* PR middle-end/87647 */

struct A {};
struct A *const b = &(struct A) {};
struct B { char *s; struct A *t; };
void bar (struct B *);

void
foo (void)
{
  struct B a[] = { "", b, "", b, "", b, "", b, "", b, "", b, "", b, "", b,
		   "", b, "", b, "", b, "", b, "", b, "", b, "", b, "", b,
		   "", b };
  bar (a);
}
