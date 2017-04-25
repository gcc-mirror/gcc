/* { dg-do compile { target int128 } } */
/* { dg-options "-O1 -mno-lra" } */

unsigned __int128 g;
void
foo ()
{
  g = (unsigned __int128)1 << 127;
}
