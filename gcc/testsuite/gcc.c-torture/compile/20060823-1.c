/* PR middle-end/28683 */

extern void foo (int *);

struct A
{
  int f;
};

struct A *
test (struct A *r)
{
  int *f = &r->f;
  static int i = 0;
  if (!i && !((void *) f == (void *) r))
    foo (&i);
  return r;
}
