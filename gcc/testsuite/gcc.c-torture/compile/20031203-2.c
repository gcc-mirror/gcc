/* Don't ICE on stupid user tricks.  */

int foo(int bar)
{
  return (&bar)[-1];
}
