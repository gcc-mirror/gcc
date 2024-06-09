/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=gnu23" } */

void
foo (_Complex int ci, _Complex long long cl)
{
  _BitInt(__SIZEOF_INT__ * __CHAR_BIT__ + 1) bi = 0wb;
  ci + bi;			/* { dg-message "unsupported" } */
  bi + ci;			/* { dg-message "unsupported" } */
#if __BITINT_MAXWIDTH__ >= 575
  _BitInt(575) bw = 0wb;
  cl + bw;			/* { dg-message "unsupported" "" { target bitint575 } } */
  bw + cl;			/* { dg-message "unsupported" "" { target bitint575 } } */
#endif
}
