/* { dg-do compile { target rx-*-* } } */
/* { dg-options "-O2 -g -fno-omit-frame-pointer" } */
struct baz;
struct foo {
  struct baz *c;
  unsigned int flags;
};
struct bar {
  const struct bar *b;
  void (*func)(struct foo *a, struct baz *c, int flags);
};
struct baz {
  int flag;
  const struct bar *b;
};
static inline
__attribute__((always_inline))
__attribute__((no_instrument_function)) void inline1(struct foo *a)
{
  a->flags |= 1;
}
static inline
__attribute__((always_inline))
__attribute__((no_instrument_function)) int inline2(struct baz *c)
{
  return c->flag == 1;
}
extern const struct bar _bar;
extern void func(struct foo *a);
void pr89828(struct foo *a, struct baz *c, int flags)
{
  const struct bar *b;

  if (c->b == a->c->b) {
    a->c->b->func(a, c, flags);
  } else {
    for (b = (&_bar); b; b = b->b) {
      if (b == a->c->b)
	break;
      if (b == c->b) {
	func(a);
	break;
      }
    }
  }

  if (inline2(a->c))
    inline1(a);
}
