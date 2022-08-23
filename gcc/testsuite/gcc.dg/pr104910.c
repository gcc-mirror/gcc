/* PR target/104910 */
/* { dg-do compile } */
/* { dg-options "-Os -fno-forward-propagate" } */
/* { dg-additional-options "-fstack-protector-all" { target fstack_protector } } */

void
bar (void);

void
foo (int x)
{
  if (x)
    bar ();
}
