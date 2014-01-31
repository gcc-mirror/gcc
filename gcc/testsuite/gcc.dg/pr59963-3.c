/* PR c/59963 */
/* { dg-do compile } */
/* { dg-options "-Wconversion" } */

extern void foo (void *p);

void
bar (void)
{
  {
    /* This must not ICE.  */
    int i __attribute__((cleanup (foo)));
  }
}
