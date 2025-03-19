/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

/* During combine we may end up with patterns of the form

   (set (reg:DI 66 [ _6 ])
        (ashift:DI (reg:DI 72 [ x ])
                   (subreg:QI (and:TI (reg:TI 67 [ _1 ])
                                      (const_wide_int 0x0aaaaaaaaaaaaaabf))
                              15)))

   which should be rejected since the shift count does not trivially fit the
   scheme of address operands.  */

long
test (long x, int y)
{
  __int128 z = 0xAAAAAAAAAAAAAABF;
  z &= y;
  return x << z;
}
