/* { dg-do run } */
/* { dg-options "-O" } */

extern void abort (void);

typedef struct {
  int b, c;
}
__attribute__((aligned(32))) inner_t; // data type is 32 byte aligned

typedef struct {
  inner_t *inner;
  int a;
} outer_t;

void __attribute__  ((noinline,weak))
somefunc (int a, int b, int c)
{
  if (!a || !b || c)
    abort ();
};

__attribute__  ((noinline,weak))
outer_t *
some_alloc_1 ()
{
  static outer_t x;
  return &x;
}

__attribute__  ((noinline,weak))
inner_t *
some_alloc_2 ()
{
  static inner_t x;
  return &x;
}

int main (void)
{
  int y, y2, y3;
  // @p_out is pointing to instance of outer_t, naturally aligned to 4+4 = 8
  // and not gauranteed be 32 byte aligned.
  outer_t *p_out = some_alloc_1( ); // returns 8 byte aligned ptr

  // @ptr is pointing to instance of inner_t which is naturally aligned to 32.
  // It is assigned to p_out->inner which is of type inner_t thus 32 byte
  // aligned as well
  // Note that gcc can deduce p_out->inner is 32b aligned, not at runtime,
  // because it was assigned @ptr, but even at compile time, because it's data
  // type is naturally 32 byte aligned.
  inner_t *ptr = some_alloc_2(); // returns 32 byte aligned ptr
  p_out->inner = ptr; // this ptr will also be 32 byte aligned

  y = __builtin_arc_aligned(ptr, 32); // this shd return 1
  y2 = __builtin_arc_aligned(p_out->inner, 32); // this also shd return 1
  // Although p_out->inner ptr is 32 byte aligned,
  // it's container &(p_out->inner) need not be.
  // That is because the hoister has no relation to contents.
  // p_out is not gauranteed to be 32 byte
  // aligned, so it's member @inner in p_out need not be.
  y3 = __builtin_arc_aligned(&(p_out->inner), 32);
  // compiler not sure, so must return 0

  somefunc(y, y2, y3);
  return 0;
}
