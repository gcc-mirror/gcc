/* PR target/104448 */
/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-mavx5124vnniw -mno-xsave -mabi=ms" } */
/* { dg-warning "AVX5124VNNIW support will be removed in GCC 15" "" { target *-*-* } 0 } */

int
main ()
{
  return 0;
}
