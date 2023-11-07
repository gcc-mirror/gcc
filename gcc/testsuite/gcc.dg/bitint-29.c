/* PR c/102989 */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "-std=c23" } */
/* { dg-add-options ieee } */

#include <fenv.h>

#if __FLT_MANT_DIG__ == 24
#if __BITINT_MAXWIDTH__ >= 135
__attribute__((noipa)) _BitInt(135)
testflt_135 (float d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(135)
testfltu_135 (float d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 192
__attribute__((noipa)) _BitInt(192)
testflt_192 (float d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(192)
testfltu_192 (float d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
__attribute__((noipa)) _BitInt(575)
testflt_575 (float d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(575)
testfltu_575 (float d)
{
  return d;
}
#endif
#endif

#if __DBL_MANT_DIG__ == 53
#if __BITINT_MAXWIDTH__ >= 135
__attribute__((noipa)) _BitInt(135)
testdbl_135 (double d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(135)
testdblu_135 (double d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 192
__attribute__((noipa)) _BitInt(192)
testdbl_192 (double d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(192)
testdblu_192 (double d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
__attribute__((noipa)) _BitInt(575)
testdbl_575 (double d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(575)
testdblu_575 (double d)
{
  return d;
}
#endif
#endif

#if __LDBL_MANT_DIG__ == 64
#if __BITINT_MAXWIDTH__ >= 135
__attribute__((noipa)) _BitInt(135)
testldbl_135 (long double d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(135)
testldblu_135 (long double d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 192
__attribute__((noipa)) _BitInt(192)
testldbl_192 (long double d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(192)
testldblu_192 (long double d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
__attribute__((noipa)) _BitInt(575)
testldbl_575 (long double d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(575)
testldblu_575 (long double d)
{
  return d;
}
#endif
#endif

#if __FLT128_MANT_DIG__ == 113
#if __BITINT_MAXWIDTH__ >= 135
__attribute__((noipa)) _BitInt(135)
testflt128_135 (_Float128 d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(135)
testflt128u_135 (_Float128 d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 192
__attribute__((noipa)) _BitInt(192)
testflt128_192 (_Float128 d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(192)
testflt128u_192 (_Float128 d)
{
  return d;
}
#endif

#if __BITINT_MAXWIDTH__ >= 575
__attribute__((noipa)) _BitInt(575)
testflt128_575 (_Float128 d)
{
  return d;
}

__attribute__((noipa)) unsigned _BitInt(575)
testflt128u_575 (_Float128 d)
{
  return d;
}
#endif
#endif

__attribute__((noipa)) void
check_inexact (int test, int inex)
{
  if (!test)
    __builtin_abort ();
  if ((!fetestexcept (FE_INEXACT)) != (!inex))
    __builtin_abort ();
  feclearexcept (FE_INEXACT);
}

int
main ()
{
#if __FLT_MANT_DIG__ == 24
#if __BITINT_MAXWIDTH__ >= 135
  check_inexact (testflt_135 (-85070591730234615865843651857942052864.0f) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testflt_135 (0xffffffp+104f) == 340282346638528859811704183484516925440wb, 0);
  check_inexact (testflt_135 (-0xffffffp+104f) == -340282346638528859811704183484516925440wb, 0);
  check_inexact (testflt_135 (-0xffffffp-1f) == -8388607wb, 1);
  check_inexact (testflt_135 (-0.f) == 0wb, 0);
  check_inexact (testflt_135 (-0.f) == 0wb, 0);
  check_inexact (testflt_135 (-0.9990234375f) == 0wb, 1);
  check_inexact (testfltu_135 (0.f) == 0uwb, 0);
  check_inexact (testfltu_135 (-0.9990234375f) == 0uwb, 1);
  check_inexact (testfltu_135 (0xffffffp-1f) == 8388607uwb, 1);
  check_inexact (testfltu_135 (0xffffffp+104f) == 340282346638528859811704183484516925440uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 192
  check_inexact (testflt_192 (-85070591730234615865843651857942052864.0f) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testflt_192 (0xffffffp+104f) == 340282346638528859811704183484516925440wb, 0);
  check_inexact (testflt_192 (-0xffffffp+104f) == -340282346638528859811704183484516925440wb, 0);
  check_inexact (testflt_192 (-0xffffffp-3f) == -2097151wb, 1);
  check_inexact (testflt_192 (-0.f) == 0wb, 0);
  check_inexact (testflt_192 (-0.9990234375f) == 0wb, 1);
  check_inexact (testfltu_192 (0.f) == 0uwb, 0);
  check_inexact (testfltu_192 (-0.9990234375f) == 0uwb, 1);
  check_inexact (testfltu_192 (0xffffffp-3f) == 2097151uwb, 1);
  check_inexact (testfltu_192 (0xffffffp+104f) == 340282346638528859811704183484516925440uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 575
  check_inexact (testflt_575 (-85070591730234615865843651857942052864.0f) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testflt_575 (0xffffffp+104f) == 340282346638528859811704183484516925440wb, 0);
  check_inexact (testflt_575 (-0xffffffp+104f) == -340282346638528859811704183484516925440wb, 0);
  check_inexact (testflt_575 (-0xffffffp-5f) == -524287wb, 1);
  check_inexact (testflt_575 (0.f) == 0wb, 0);
  check_inexact (testflt_575 (-0.9990234375f) == 0wb, 1);
  check_inexact (testfltu_575 (-0.f) == 0uwb, 0);
  check_inexact (testfltu_575 (-0.9990234375f) == 0uwb, 1);
  check_inexact (testfltu_575 (0xffffffp-5f) == 524287uwb, 1);
  check_inexact (testfltu_575 (0xffffffp+104f) == 340282346638528859811704183484516925440uwb, 0);
#endif
#endif
#if __DBL_MANT_DIG__ == 53
#if __BITINT_MAXWIDTH__ >= 135
  check_inexact (testdbl_135 (-85070591730234615865843651857942052864.0) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testdbl_135 (0x1fffffffffffffp+81) == 21778071482940059243804335646374816120832wb, 0);
  check_inexact (testdbl_135 (-0x20000000000000p+81) == -21778071482940061661655974875633165533183wb - 1, 0);
  check_inexact (testdbl_135 (-0x1fffffffffffffp-1) == -4503599627370495wb, 1);
  check_inexact (testdbl_135 (-0.) == 0wb, 0);
  check_inexact (testdbl_135 (-0.9990234375) == 0wb, 1);
  check_inexact (testdblu_135 (0.) == 0uwb, 0);
  check_inexact (testdblu_135 (-0.9990234375) == 0uwb, 1);
  check_inexact (testdblu_135 (0x1fffffffffffffp-1) == 4503599627370495uwb, 1);
  check_inexact (testdblu_135 (0x1fffffffffffffp+82) == 43556142965880118487608671292749632241664uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 192
  check_inexact (testdbl_192 (-85070591730234615865843651857942052864.0) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testdbl_192 (0x1fffffffffffffp+138) == 3138550867693340033468750984562846621555579712101368725504wb, 0);
  check_inexact (testdbl_192 (-0x20000000000000p+138) == -3138550867693340381917894711603833208051177722232017256447wb - 1, 0);
  check_inexact (testdbl_192 (-0x1fffffffffffffp-3) == -1125899906842623wb, 1);
  check_inexact (testdbl_192 (0.) == 0wb, 0);
  check_inexact (testdbl_192 (-0.9990234375) == 0wb, 1);
  check_inexact (testdblu_192 (-0.) == 0uwb, 0);
  check_inexact (testdblu_192 (-0.9990234375) == 0uwb, 1);
  check_inexact (testdblu_192 (0x1fffffffffffffp-3) == 1125899906842623uwb, 1);
  check_inexact (testdblu_192 (0x1fffffffffffffp+139) == 6277101735386680066937501969125693243111159424202737451008uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 575
  check_inexact (testdbl_575 (-85070591730234615865843651857942052864.0) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testdbl_575 (0x1fffffffffffffp+521) == 61832600368276126650327970124302082526882038193909742709080463879918896882169507607035916867654709124839777195049479857541529867095829765369898539058829479405123401922117632wb, 0);
  check_inexact (testdbl_575 (-0x20000000000000p+521) == -61832600368276133515125630254911797508782837275302959978515764023224306276632966792579100265310761247399417856504034834837841258576687802491886538775473291979151693037174783wb - 1, 0);
  check_inexact (testdbl_575 (-0x1fffffffffffffp-5) == -281474976710655wb, 1);
  check_inexact (testdbl_575 (-0.) == 0wb, 0);
  check_inexact (testdbl_575 (-0.9990234375) == 0wb, 1);
  check_inexact (testdblu_575 (0.) == 0uwb, 0);
  check_inexact (testdblu_575 (-0.9990234375) == 0uwb, 1);
  check_inexact (testdblu_575 (0x1fffffffffffffp-5) == 281474976710655uwb, 1);
  check_inexact (testdblu_575 (0x1fffffffffffffp+522) == 123665200736552253300655940248604165053764076387819485418160927759837793764339015214071833735309418249679554390098959715083059734191659530739797078117658958810246803844235264uwb, 0);
#endif
#endif
#if __LDBL_MANT_DIG__ == 64
#if __BITINT_MAXWIDTH__ >= 135
  check_inexact (testldbl_135 (-85070591730234615865843651857942052864.0L) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testldbl_135 (0xffffffffffffffffp+70L) == 21778071482940061660475383254915754229760wb, 0);
  check_inexact (testldbl_135 (-0x10000000000000000p+70L) == -21778071482940061661655974875633165533183wb - 1, 0);
  check_inexact (testldbl_135 (-0xffffffffffffffffp-1L) == -9223372036854775807wb, 1);
  check_inexact (testldbl_135 (-0.L) == 0wb, 0);
  check_inexact (testldbl_135 (-0.9990234375L) == 0wb, 1);
  check_inexact (testldblu_135 (0.L) == 0uwb, 0);
  check_inexact (testldblu_135 (-0.9990234375L) == 0uwb, 1);
  check_inexact (testldblu_135 (0xffffffffffffffffp-1L) == 9223372036854775807uwb, 1);
  check_inexact (testldblu_135 (0xffffffffffffffffp+71L) == 43556142965880123320950766509831508459520uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 192
  check_inexact (testldbl_192 (-85070591730234615865843651857942052864.0L) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testldbl_192 (0xffffffffffffffffp+127L) == 3138550867693340381747753528143363976319490418516133150720wb, 0);
  check_inexact (testldbl_192 (-0x10000000000000000p+127L) == -3138550867693340381917894711603833208051177722232017256447wb - 1, 0);
  check_inexact (testldbl_192 (-0xffffffffffffffffp-2L) == -4611686018427387903wb, 1);
  check_inexact (testldbl_192 (0.L) == 0wb, 0);
  check_inexact (testldbl_192 (-0.9990234375L) == 0wb, 1);
  check_inexact (testldblu_192 (-0.L) == 0uwb, 0);
  check_inexact (testldblu_192 (-0.9990234375L) == 0uwb, 1);
  check_inexact (testldblu_192 (0xffffffffffffffffp-2L) == 4611686018427387903uwb, 1);
  check_inexact (testldblu_192 (0xffffffffffffffffp+128L) == 6277101735386680763495507056286727952638980837032266301440uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 575
  check_inexact (testldbl_575 (-85070591730234615865843651857942052864.0L) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testldbl_575 (0xffffffffffffffffp+510L) == 61832600368276133511773678272426148233889331025751498446645922568076207932202076431648659257792374503198949281962308977915333294030066289778448068072486649492543280785653760wb, 0);
  check_inexact (testldbl_575 (-0x10000000000000000p+510L) == -61832600368276133515125630254911797508782837275302959978515764023224306276632966792579100265310761247399417856504034834837841258576687802491886538775473291979151693037174783wb - 1, 0);
  check_inexact (testldbl_575 (-0xffffffffffffffffp-4L) == -1152921504606846975wb, 1);
  check_inexact (testldbl_575 (0.L) == 0wb, 0);
  check_inexact (testldbl_575 (-0.9990234375L) == 0wb, 1);
  check_inexact (testldblu_575 (-0.L) == 0uwb, 0);
  check_inexact (testldblu_575 (-0.9990234375L) == 0uwb, 1);
  check_inexact (testldblu_575 (0xffffffffffffffffp-4L) == 1152921504606846975uwb, 1);
  check_inexact (testldblu_575 (0xffffffffffffffffp+511L) == 123665200736552267023547356544852296467778662051502996893291845136152415864404152863297318515584749006397898563924617955830666588060132579556896136144973298985086561571307520uwb, 0);
#endif
#endif
#if __FLT128_MANT_DIG__ == 113
#if __BITINT_MAXWIDTH__ >= 135
  check_inexact (testflt128_135 (-85070591730234615865843651857942052864.0F128) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testflt128_135 (0x1ffffffffffffffffffffffffffffp+21F128) == 21778071482940061661655974875633163436032wb, 0);
  check_inexact (testflt128_135 (-0x20000000000000000000000000000p+21F128) == -21778071482940061661655974875633165533183wb - 1, 0);
  check_inexact (testflt128_135 (-0x1ffffffffffffffffffffffffffffp-1F128) == -5192296858534827628530496329220095wb, 1);
  check_inexact (testflt128_135 (-0.F128) == 0wb, 0);
  check_inexact (testflt128_135 (-0.9990234375F128) == 0wb, 1);
  check_inexact (testflt128u_135 (0.F128) == 0uwb, 0);
  check_inexact (testflt128u_135 (-0.9990234375F128) == 0uwb, 1);
  check_inexact (testflt128u_135 (0x1ffffffffffffffffffffffffffffp-1F128) == 5192296858534827628530496329220095uwb, 1);
  check_inexact (testflt128u_135 (0x1ffffffffffffffffffffffffffffp+22F128) == 43556142965880123323311949751266326872064uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 192
  check_inexact (testflt128_192 (-85070591730234615865843651857942052864.0F128) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testflt128_192 (0x1ffffffffffffffffffffffffffffp+78F128) == 3138550867693340381917894711603832905819722818574723579904wb, 0);
  check_inexact (testflt128_192 (-0x20000000000000000000000000000p+78F128) == -3138550867693340381917894711603833208051177722232017256447wb - 1, 0);
  check_inexact (testflt128_192 (-0x1ffffffffffffffffffffffffffffp-4F128) == -649037107316853453566312041152511wb, 1);
  check_inexact (testflt128_192 (-0.F128) == 0wb, 0);
  check_inexact (testflt128_192 (-0.9990234375F128) == 0wb, 1);
  check_inexact (testflt128u_192 (0.F128) == 0uwb, 0);
  check_inexact (testflt128u_192 (-0.9990234375F128) == 0uwb, 1);
  check_inexact (testflt128u_192 (0x1ffffffffffffffffffffffffffffp-4F128) == 649037107316853453566312041152511uwb, 1);
  check_inexact (testflt128u_192 (0x1ffffffffffffffffffffffffffffp+79F128) == 6277101735386680763835789423207665811639445637149447159808uwb, 0);
#endif
#if __BITINT_MAXWIDTH__ >= 575
  check_inexact (testflt128_575 (-85070591730234615865843651857942052864.0F128) == -85070591730234615865843651857942052864wb, 0);
  check_inexact (testflt128_575 (0x1ffffffffffffffffffffffffffffp+461F128) == 61832600368276133515125630254911791554520007845691312598455129804691160851602940042069550439343049559602369631548246946680753811425558728725309540242943660463695151425912832wb, 0);
  check_inexact (testflt128_575 (-0x20000000000000000000000000000p+461F128) == -61832600368276133515125630254911797508782837275302959978515764023224306276632966792579100265310761247399417856504034834837841258576687802491886538775473291979151693037174783wb - 1, 0);
  check_inexact (testflt128_575 (-0x1ffffffffffffffffffffffffffffp-8F128) == -40564819207303340847894502572031wb, 1);
  check_inexact (testflt128_575 (0.F128) == 0wb, 0);
  check_inexact (testflt128_575 (-0.9990234375F128) == 0wb, 1);
  check_inexact (testflt128u_575 (-0.F128) == 0uwb, 0);
  check_inexact (testflt128u_575 (-0.9990234375F128) == 0uwb, 1);
  check_inexact (testflt128u_575 (0x1ffffffffffffffffffffffffffffp-8F128) == 40564819207303340847894502572031uwb, 1);
  check_inexact (testflt128u_575 (0x1ffffffffffffffffffffffffffffp+462F128) == 123665200736552267030251260509823583109040015691382625196910259609382321703205880084139100878686099119204739263096493893361507622851117457450619080485887320927390302851825664uwb, 0);
#endif
#endif
}
