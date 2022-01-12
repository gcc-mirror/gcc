/* PR target/103915 */
/* { dg-do compile } */
/* { dg-options "-flive-range-shrinkage" } */

char __attribute__((__vector_size__ (2))) data;

void
foo (void)
{
  data = ~data;
}
