/* PR c/61553 */
/* { dg-do compile } */

void
foo (char *s)
{
  __atomic_store (s, (void *) 0, __ATOMIC_SEQ_CST); /* { dg-error "size mismatch" } */
}
