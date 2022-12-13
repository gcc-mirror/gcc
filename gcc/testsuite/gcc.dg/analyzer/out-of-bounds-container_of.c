/* Further reduced container_of pattern from the Linux Kernel.  */

struct inner {
  /* Don't care */
};

struct outer {
  int i;
  struct inner inner_struct;
};

struct outer *container_of (struct inner *ptr_to_inner)
{
  struct outer *ptr_to_outer = ((struct outer *) (((void *) ptr_to_inner) - __builtin_offsetof(struct outer, inner_struct)));
  return ptr_to_outer;
}

int test (struct outer *outer_p, struct inner *inner_p)
{
  struct outer test;
  test.i = 42;
  struct inner test2;
  int sum = 0;
  struct outer *o;

  /* Symbolic inner struct.  */
  o = container_of (inner_p);
  sum += o->i; // ok
  /* Not ok, but we can't be sure that outer
     is actually the container of inner.  */
  sum += (o - 1)->i;
  /* Symbolic outer struct.  */
  o = container_of (&(outer_p->inner_struct));
  sum += o->i; // ok
  /* Not ok, but indistinguishable from the case above.  */
  sum += (o - 1)->i;
  /* Concrete outer struct.  */
  o = container_of (&(test.inner_struct));
  sum += o->i;  // ok
  /* Not ok and we do have a concrete region.  */
  sum += (o - 1)->i; /* { dg-line testA } */
  /* Concrete inner struct, has no container.  */
  o = container_of (&test2);
  sum += o->i; /* { dg-line testB } */

  return sum;
  /* { dg-warning "stack-based buffer under-read" "warning" { target *-*-* } testA } */
  /* { dg-message "" "note" { target *-*-* } testA } */
  /* { dg-warning "stack-based buffer under-read" "warning" { target *-*-* } testB } */
  /* { dg-message "" "note" { target *-*-* } testB } */
}
