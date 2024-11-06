/* PR c/116735  */
/* { dg-options "-std=c99" } */
/* { dg-do compile } */

struct foo {
  int len;
  int element[] __attribute__ ((__counted_by__ (lenx))); /* { dg-error "attribute is not a field declaration in the same structure as" } */
};

struct bar {
  float count;
  int array[] __attribute ((counted_by (count))); /* { dg-error "attribute is not a field declaration with an integer type" } */
};

int main ()
{
  struct foo *p = __builtin_malloc (sizeof (struct foo) + 3 * sizeof (int));
  struct bar *q = __builtin_malloc (sizeof (struct bar) + 3 * sizeof (int));
  p->len = 3;
  p->element[0] = 17;
  p->element[1] = 13;
  q->array[0] = 13;
  q->array[2] = 17;
  return 0;
}
