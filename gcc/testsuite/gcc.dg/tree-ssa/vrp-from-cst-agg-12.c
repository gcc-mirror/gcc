/* { dg-do run } */
/* { dg-options "-O2" } */

/* A load from a constant aggregate whose pointers are all null, or left
   out of its initializer, is null.  One that may read a null or a
   non-null pointer is either.  */

void link_error (void);

static void handle (void) { }

struct op { int code; void (*handler) (void); };
static const struct op ops[3] = { { 1, 0 }, { 2, 0 }, { 3, 0 } };
static void (*const fns[4]) (void) = { 0 };
static void (*const mixed[2]) (void) = { 0, handle };

[[gnu::noinline]] void
foo (int i)
{
  if (ops[i].handler)
    link_error ();
}

[[gnu::noinline]] void
bar (int i)
{
  if (fns[i])
    link_error ();
}

[[gnu::noipa]] int
baz (int i)
{
  return mixed[i] != 0;
}

[[gnu::noipa]] int
qux (int i, int c)
{
  void (*p) (void) = c ? mixed[i] : handle;
  return p != 0;
}

[[gnu::noipa]] int
get_index (void)
{
  return 1;
}

int
main (void)
{
  foo (get_index ());
  bar (get_index ());
  if (baz (0) || !baz (1))
    __builtin_abort ();
  if (qux (0, 1) || !qux (1, 1))
    __builtin_abort ();
  return 0;
}
