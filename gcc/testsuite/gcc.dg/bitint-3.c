/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_BitInt(32) a, b = 2147483647wb;
_BitInt(64) c, d = 9223372036854775807wb;
struct S {
  _BitInt(2) a;
  _BitInt(6) b;
} s, t = { -1wb - 1wb, 31wb };

void
foo (void)
{
  a = b;
  c = d;
  s.a = t.a;
  s.b = t.b;
}

void
bar (void)
{
  a += b;
  c += d;
  s.a += t.a;
  s.b += t.b;
}

#if __BITINT_MAXWIDTH__ >= 128
unsigned _BitInt(128) e = 340282366920938463463374607431768211455uwb;
_BitInt(128) f = 20000000000000000000000000000000000000wb;
_BitInt(128) g = -20000000000000000000000000000000000000wb;
#endif

#if __BITINT_MAXWIDTH__ >= 575
unsigned _BitInt(575) h = 123665200736552267030251260509823595017565674550605919957031528046448612553265933585158200530621522494798835713008069669675682517153375604983773077550946583958303386074349567uwb;
_BitInt(575) i = 2000000000000000000000000000000000000000000wb;
_BitInt(575) j = -2000000000000000000000000000000000000000000wb;
#endif
