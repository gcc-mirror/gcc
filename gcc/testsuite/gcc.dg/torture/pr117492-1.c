/* { dg-do compile } */
/* PR middle-end/117492 */

/* This code would ICE in fold due to code which was using wi::to_wide with different types
   and adding them. */

typedef unsigned u;

u
foo(u x)
{
  return
    __builtin_stdc_rotate_left((unsigned)
      __builtin_stdc_rotate_right(x, 0x100000001ull),
                                  1);
}
