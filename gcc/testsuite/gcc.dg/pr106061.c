/* PR ipa/106061 */
/* { dg-do compile } */
/* { dg-options "-Og" } */

extern void foo (void);

inline void
bar (int x)
{
  if (x)
    foo ();
}

void
baz (void)
{
  bar (0);
}
