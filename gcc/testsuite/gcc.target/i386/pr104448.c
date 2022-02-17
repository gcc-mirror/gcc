/* PR target/104448 */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-mavx5124vnniw -mno-xsave -mabi=ms" } */

int
main ()
{
  return 0;
}
