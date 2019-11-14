/* PR target/92449 */
/* { dg-additional-options "-ffast-math -fno-cx-limited-range" } */

void do_div (_Complex double *a, _Complex double *b)
{
  *a = *b / (4.0 - 5.0fi);
}
