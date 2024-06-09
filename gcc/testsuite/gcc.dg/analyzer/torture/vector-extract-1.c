/* PR analyzer/113983  */
/* { dg-additional-options "-Wno-psabi" } */

/* maybe_undo_optimize_bit_field_compare used to ICE on this
   because it was not checking for only integer types. */

typedef int __attribute__((__vector_size__(8))) V;
int i;

V
foo(void)
{
  V v = (V){};
  return (0, 0) * (i & v);
}
