/* PR c/102989 */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)

#ifdef __BITINT_MAXWIDTH__
void
foo (void)
{
  _BitInt(42) a = (_BitInt(42)) 1;
  _BitInt(42) unsigned b = (unsigned _BitInt(42)) 2;
  _BitInt(5) c = (_BitInt(5)) 3;
  _BitInt(5) unsigned d = (unsigned _BitInt(5)) 4;
  _BitInt(32) e = (_BitInt(32)) 5;
  _BitInt(32) unsigned f = (unsigned _BitInt(32)) 6;
  constexpr int g = 43;
  enum E { F = 44 };
  _BitInt(g) h;
  unsigned _BitInt(F) i;
  static_assert (expr_has_type (a, signed _BitInt(42)), "");
  static_assert (expr_has_type (a, _BitInt(42)), "");
  static_assert (!expr_has_type (a, unsigned _BitInt(42)), "");
  static_assert (!expr_has_type (b, signed _BitInt(42)), "");
  static_assert (!expr_has_type (b, _BitInt(42)), "");
  static_assert (expr_has_type (b, unsigned _BitInt(42)), "");
  static_assert (expr_has_type (a + b, unsigned _BitInt(42)), "");
  static_assert (expr_has_type (a * b, unsigned _BitInt(42)), "");
  static_assert (expr_has_type (a - b, unsigned _BitInt(42)), "");
  static_assert (expr_has_type (a * a, _BitInt(42)), "");
  static_assert (expr_has_type (a / b, unsigned _BitInt(42)), "");
  static_assert (expr_has_type (c, signed _BitInt(5)), "");
  static_assert (expr_has_type (c, _BitInt(5)), "");
  static_assert (!expr_has_type (c, unsigned _BitInt(5)), "");
  static_assert (!expr_has_type (d, signed _BitInt(5)), "");
  static_assert (!expr_has_type (d, _BitInt(5)), "");
  static_assert (expr_has_type (d, unsigned _BitInt(5)), "");
  static_assert (expr_has_type (c + d, unsigned _BitInt(5)), "");
  static_assert (expr_has_type (c * d, unsigned _BitInt(5)), "");
  static_assert (expr_has_type (c - d, unsigned _BitInt(5)), "");
  static_assert (expr_has_type (c * c, _BitInt(5)), "");
  static_assert (expr_has_type (c / d, unsigned _BitInt(5)), "");
  static_assert (expr_has_type (e, signed _BitInt(32)), "");
  static_assert (expr_has_type (e, _BitInt(32)), "");
  static_assert (!expr_has_type (e, unsigned _BitInt(32)), "");
  static_assert (!expr_has_type (f, signed _BitInt(32)), "");
  static_assert (!expr_has_type (f, _BitInt(32)), "");
  static_assert (expr_has_type (f, unsigned _BitInt(32)), "");
  static_assert (expr_has_type (e + f, unsigned _BitInt(32)), "");
  static_assert (expr_has_type (e * f, unsigned _BitInt(32)), "");
  static_assert (expr_has_type (e - f, unsigned _BitInt(32)), "");
  static_assert (expr_has_type (e * e, _BitInt(32)), "");
  static_assert (expr_has_type (e / f, unsigned _BitInt(32)), "");
#if __CHAR_BIT__ * __SIZEOF_INT__ == 32
  static_assert (expr_has_type (e + 1, int), "");
  static_assert (expr_has_type (f + 1, unsigned int), "");
  static_assert (expr_has_type (e + 1U, unsigned int), "");
  static_assert (expr_has_type (f + 1U, unsigned int), "");
  static_assert (expr_has_type (1 - e, int), "");
  static_assert (expr_has_type (1 - f, unsigned int), "");
  static_assert (expr_has_type (1U * e, unsigned int), "");
  static_assert (expr_has_type (1U / f, unsigned int), "");
#endif
  static_assert (expr_has_type (0wb, _BitInt(2)), "");
  static_assert (expr_has_type (-1WB, _BitInt(2)), "");
  static_assert (expr_has_type (-1wb - 1wb, _BitInt(2)), "");
  static_assert (expr_has_type (1wb, _BitInt(2)), "");
  static_assert (expr_has_type (2wb, _BitInt(3)), "");
  static_assert (expr_has_type (0wbu, unsigned _BitInt(1)), "");
  static_assert (expr_has_type (-1UWB, unsigned _BitInt(1)), "");
  static_assert (expr_has_type (1uWB, unsigned _BitInt(1)), "");
  static_assert (expr_has_type (2Uwb, unsigned _BitInt(2)), "");
  static_assert (expr_has_type (h, signed _BitInt(43)), "");
  static_assert (expr_has_type (i, unsigned _BitInt(44)), "");
  static_assert (0wb == 0, "");
  static_assert (-1wb == -1, "");
  static_assert (0xffffffffwb == 4294967295wb, "");
  static_assert (0xffffffffwb == 000000000000000000000000000037777777777wb, "");
  static_assert (0x0000000000000000000000000ffffffffwb == 0b00011111111111111111111111111111111wb, "");
#if __BITINT_MAXWIDTH__ >= 128
  static_assert (expr_has_type (0xffffffffffffffffffffffffffffffffuwb, unsigned _BitInt(128)), "");
  static_assert (0xffffffffffffffffffffffffffffffffuwb == 340282366920938463463374607431768211455uwb, "");
  static_assert (200000000000000000000000000000000000000wb == 0x96769950b50d88f41314448000000000WB, "");
  static_assert (02263551452055206610750114242110000000000000wb == 200000000000000000000000000000000000000wb, "");
#endif
#if __BITINT_MAXWIDTH__ >= 575
  static_assert (expr_has_type (123665200736552267030251260509823595017565674550605919957031528046448612553265933585158200530621522494798835713008069669675682517153375604983773077550946583958303386074349567UWB, unsigned _BitInt(575)), "");
  static_assert (expr_has_type (0x00000007fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwbu, unsigned _BitInt(575)), "");
  static_assert (expr_has_type (0377777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777uwb, unsigned _BitInt(575)), "");
  static_assert (123665200736552267030251260509823595017565674550605919957031528046448612553265933585158200530621522494798835713008069669675682517153375604983773077550946583958303386074349567uwb == 0x00000007fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwbu, "");
  static_assert (0377777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777WBU == 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffuWB, "");
  static_assert (100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000uwb == 0X67815961'0903F797'4A7CB3F2'2F01FA5D'9A2CD603'9DE4A7A6'0F713676'CE7D7113'FF1A499D'9BFD0FDC'D2999652'EB4C7F39'8DE2A000'00000000'00000000'00000000'00000000'00000000UWB, "");
  static_assert (expr_has_type (-61'832600368'276133515'125630254'911797508'782837275'302959978'515764023'224306276'632966792'579100265'310761247'399417856'504034834'837841258'576687802'491886538'775473291'979151693'037174783wb, _BitInt(575)), "");
  static_assert (expr_has_type (0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwb, signed _BitInt(575)), "");
  static_assert (expr_has_type (0177777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777wb, _BitInt(575)), "");
  static_assert (-61832600368276133515125630254911797508782837275302959978515764023224306276632966792579100265310761247399417856504034834837841258576687802491886538775473291979151693037174783WB == -0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwb, "");
  static_assert (0177777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777wb / 2wb * 2wb + 1wb == 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffwb, "");
#endif
#if (0wb != 0) || (0wbU != 0U) || (1wb != 1) || (1uWB != 1u) \
    || (-1wb >= 0) || (-1UWB < 0) || (4294967295uwb != 4294967295U) \
    || 18446744073709551615wbu != 18446744073709551615ULL
#error ""
#endif
}

void
bar (void)
{
  _BitInt(2) a = (_BitInt(2)) 1;
  unsigned _BitInt(1) b = 1uwb;
  _BitInt(4) c;
  c = 7;
  _BitInt(2) d = 1;
  _BitInt(5) e = 2wb;
  unsigned _BitInt(6) f = 3uwb;
}
#endif

int
main ()
{
}
