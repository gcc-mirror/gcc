/* PR target/25554 */
/* Bitwise shift with negative shift count has undefined behavior,
   but we shouldn't ICE on it.  */

void bar (void);

void
foo (long x)
{
  if (((x >> -2) & 1) != 0)
    bar ();
}
