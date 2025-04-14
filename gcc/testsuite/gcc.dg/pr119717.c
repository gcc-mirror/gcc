/* PR c/119717  */
/* { dg-additional-options "-std=c23" } */
/* { dg-do compile } */

struct annotated {
  unsigned count;
  [[gnu::counted_by(count)]] char array[];
};

[[gnu::noinline,gnu::noipa]]
static unsigned
size_of (bool x, struct annotated *a)
{
  char *p = (x ? a : 0)->array;
  return __builtin_dynamic_object_size (p, 1);
}

int main()
{
  struct annotated *p = __builtin_malloc(sizeof *p);
  p->count = 0;
  __builtin_printf ("the bdos whole is %ld\n", size_of (0, p));
  return 0;
}
