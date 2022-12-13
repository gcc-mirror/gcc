/* PR target/106577 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mavx" } */

int i;
void
foo (void)
{
  i ^= !(((unsigned __int128)0xf0f0f0f0f0f0f0f0 << 64 | 0xf0f0f0f0f0f0f0f0) & i);
}
