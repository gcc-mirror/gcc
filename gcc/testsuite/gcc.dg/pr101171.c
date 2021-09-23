/* PR c/101171 */
/* { dg-do compile } */
/* { dg-options "" } */

extern void foo (void);
int x = 0x1234;

void
bar (void)
{
  if (x != (sizeof ((enum T) 0x1234)))	/* { dg-error "conversion to incomplete type" } */
    foo ();
}
