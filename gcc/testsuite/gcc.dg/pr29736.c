/* PR c/29736 */

int __attribute__ ((vector_size (8), vector_size (8))) v; /* { dg-error "invalid vector type" } */

extern int __attribute__ ((vector_size (8))) w;
int __attribute__ ((vector_size (8))) w;

void
foo ()
{
  v = v + v;
  w = w + w;
}
