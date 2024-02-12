/* PR c++/113674 */
/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "" } */

[[____noreturn____]] int foo (int i)		/* { dg-warning "'__noreturn__' attribute (directive )?ignored" } */
{
  return i;
}

[[____maybe_unused____]] int bar (int i)	/* { dg-warning "'__maybe_unused__' attribute (directive )?ignored" } */
{
  return i;
}
