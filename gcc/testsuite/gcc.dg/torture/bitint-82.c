/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 532
_BitInt(5) a = 2, b = -2;
_BitInt(38) c = 12345, d = -12345;
_BitInt(129) e = 147090211948845388976606115811401318743wb, f = -147090211948845388976606115811401318743wb;
_BitInt(532) g = 34476769918317100226195145251004381172591594205376273814wb, h = -102116935649428556311918486808926113041433456371844211259677321wb;
unsigned _BitInt(1) i = 1;
unsigned _BitInt(17) j = 49127uwb;
unsigned _BitInt(60) k = 588141367522129848uwb;
unsigned _BitInt(205) l = 33991671979236490040668305838261113909013362173682935296620088uwb;
unsigned _BitInt(475) z = 14834685124553929878903720794923785539321715423294864257448721201977655202426343038777008759878862591302200019811097993772912691139803983786083uwb;
#endif

#include "../bitintext.h"

#if __BITINT_MAXWIDTH__ >= 532
[[gnu::noipa]] _BitInt(217)
f1 (_BitInt(9) a, unsigned _BitInt(12) b, _BitInt(36) c, unsigned _BitInt(105) d,
    _BitInt(135) e, unsigned _BitInt(168) f, _BitInt(207) g, _BitInt(207) h,
    unsigned _BitInt(531) i, _BitInt(36) j)
{
  BEXTC (a); BEXTC (b); BEXTC (c); BEXTC (d);
  BEXTC (e); BEXTC (f); BEXTC (g); BEXTC (h);
  BEXTC (i); BEXTC (j);
  _BitInt(9) k = a + 1;
  unsigned _BitInt(12) l = b - a;
  _BitInt(36) m = c * j;
  unsigned _BitInt(105) n = d >> (-2 * j);
  _BitInt(135) o = e | -j;
  unsigned _BitInt(168) p = f & 101010101010101010101010uwb;
  _BitInt(207) q = g * j;
  _BitInt(207) r = g + h;
  unsigned _BitInt(531) s = i / j;
  BEXTC (k); BEXTC (l); BEXTC (m); BEXTC (n);
  BEXTC (o); BEXTC (p); BEXTC (q); BEXTC (r);
  BEXTC (s);
  unsigned _BitInt(105) t = d << (38 - j);
  BEXTC (t);
  _Atomic _BitInt(5) u = 15;
  u += 8U;
  BEXTC (u);
  _BitInt(135) v = e << 28;
  BEXTC (v);
  unsigned _BitInt(475) w = z << (29 + j);
  BEXTC (w);
  return a + 4;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 532
  BEXTC (a); BEXTC (b);
  BEXTC (c); BEXTC (d);
  BEXTC (e); BEXTC (f);
  BEXTC (g); BEXTC (h);
  BEXTC (i);
  BEXTC (j);
  BEXTC (k);
  BEXTC (l);
  BEXTC (z);
  {
    _BitInt(5) a = 2, b = -2;
    _BitInt(38) c = 12345, d = -12345;
    _BitInt(129) e = 147090211948845388976606115811401318743wb, f = -147090211948845388976606115811401318743wb;
    _BitInt(532) g = 34476769918317100226195145251004381172591594205376273814wb, h = -102116935649428556311918486808926113041433456371844211259677321wb;
    unsigned _BitInt(1) i = 1;
    unsigned _BitInt(17) j = 49127uwb;
    unsigned _BitInt(60) k = 588141367522129848uwb;
    unsigned _BitInt(205) l = 33991671979236490040668305838261113909013362173682935296620088uwb;
    BEXTC (a); BEXTC (b);
    BEXTC (c); BEXTC (d);
    BEXTC (e); BEXTC (f);
    BEXTC (g); BEXTC (h);
    BEXTC (i);
    BEXTC (j);
    BEXTC (k);
    BEXTC (l);
  }
  _BitInt(217) m = f1 (57wb, 3927uwb, 10625699364wb, 23030359755638571619326514462579uwb,
		       20797625176303404170317957140841712396356wb,
		       111831871006433449872067089878311637796827405335256uwb,
		       64853652491049541618437564623858346454131583900201311683495230wb,
		       25108562626494976011700565632680191924545340440636663075662700wb,
		       6366583146545926097709747296452085257498446783797668089081516596003270602920229800152065594152964557479773813310423759077951305431130758723519892452009351743676uwb,
		       -1);
  BEXTC (m);
#endif
}
