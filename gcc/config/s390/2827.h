/* Scheduling description for zEC12.
   Copyright (C) 2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

static
/* std::array::operator[] is not constexpr in C++14.  */
#if __cplusplus > 201402L
constexpr
#else
const
#endif
auto sched_descr_zEC12 = []{
  using T = s390_sched_insn_descr;
  std::array<T, ATTR_ENUM_MNEMONIC_COUNT> a{};
  a[MNEMONIC_ALC] = T{}.set_groupalone ();
  a[MNEMONIC_ALCG] = T{}.set_groupalone ();
  a[MNEMONIC_ALCGR] = T{}.set_groupalone ();
  a[MNEMONIC_ALCR] = T{}.set_groupalone ();
  a[MNEMONIC_AXBR] = T{}.set_groupalone ();
  a[MNEMONIC_AXTR] = T{}.set_groupalone ();
  a[MNEMONIC_BASR] = T{}.set_cracked ();
  a[MNEMONIC_BCR_FLUSH] = T{}.set_groupalone ();
  a[MNEMONIC_BRAS] = T{}.set_cracked ();
  a[MNEMONIC_BRASL] = T{}.set_cracked ();
  a[MNEMONIC_CDFBR] = T{}.set_cracked ();
  a[MNEMONIC_CDFTR] = T{}.set_cracked ();
  a[MNEMONIC_CDGBR] = T{}.set_cracked ();
  a[MNEMONIC_CDGTR] = T{}.set_cracked ();
  a[MNEMONIC_CDLFBR] = T{}.set_cracked ();
  a[MNEMONIC_CDLFTR] = T{}.set_cracked ();
  a[MNEMONIC_CDLGBR] = T{}.set_cracked ();
  a[MNEMONIC_CDLGTR] = T{}.set_cracked ();
  a[MNEMONIC_CDSG] = T{}.set_expanded ();
  a[MNEMONIC_CEFBR] = T{}.set_cracked ();
  a[MNEMONIC_CEGBR] = T{}.set_cracked ();
  a[MNEMONIC_CELFBR] = T{}.set_cracked ();
  a[MNEMONIC_CELGBR] = T{}.set_cracked ();
  a[MNEMONIC_CFDBR] = T{}.set_cracked ();
  a[MNEMONIC_CFEBR] = T{}.set_cracked ();
  a[MNEMONIC_CFXBR] = T{}.set_cracked ();
  a[MNEMONIC_CGDBR] = T{}.set_cracked ();
  a[MNEMONIC_CGDTR] = T{}.set_cracked ();
  a[MNEMONIC_CGEBR] = T{}.set_cracked ();
  a[MNEMONIC_CGXBR] = T{}.set_cracked ();
  a[MNEMONIC_CGXTR] = T{}.set_cracked ();
  a[MNEMONIC_CHHSI] = T{}.set_cracked ();
  a[MNEMONIC_CLC] = T{}.set_cracked ().set_groupalone ();
  a[MNEMONIC_CLFDBR] = T{}.set_cracked ();
  a[MNEMONIC_CLFDTR] = T{}.set_cracked ();
  a[MNEMONIC_CLFEBR] = T{}.set_cracked ();
  a[MNEMONIC_CLFXBR] = T{}.set_cracked ();
  a[MNEMONIC_CLFXTR] = T{}.set_cracked ();
  a[MNEMONIC_CLGDBR] = T{}.set_cracked ();
  a[MNEMONIC_CLGDTR] = T{}.set_cracked ();
  a[MNEMONIC_CLGEBR] = T{}.set_cracked ();
  a[MNEMONIC_CLGXBR] = T{}.set_cracked ();
  a[MNEMONIC_CLGXTR] = T{}.set_cracked ();
  a[MNEMONIC_CPSDR] = T{}.set_cracked ();
  a[MNEMONIC_CS] = T{}.set_cracked ();
  a[MNEMONIC_CSG] = T{}.set_cracked ();
  a[MNEMONIC_CXFBR] = T{}.set_cracked ();
  a[MNEMONIC_CXFTR] = T{}.set_cracked ();
  a[MNEMONIC_CXGBR] = T{}.set_cracked ();
  a[MNEMONIC_CXGTR] = T{}.set_cracked ();
  a[MNEMONIC_CXLFBR] = T{}.set_cracked ();
  a[MNEMONIC_CXLFTR] = T{}.set_cracked ();
  a[MNEMONIC_CXLGBR] = T{}.set_cracked ();
  a[MNEMONIC_CXLGTR] = T{}.set_cracked ();
  a[MNEMONIC_DLG] = T{}.set_expanded ();
  a[MNEMONIC_DLGR] = T{}.set_expanded ();
  a[MNEMONIC_DSG] = T{}.set_expanded ();
  a[MNEMONIC_DSGF] = T{}.set_expanded ();
  a[MNEMONIC_DSGFR] = T{}.set_expanded ();
  a[MNEMONIC_DSGR] = T{}.set_expanded ();
  a[MNEMONIC_DXBR] = T{}.set_groupalone ();
  a[MNEMONIC_DXTR] = T{}.set_groupalone ();
  a[MNEMONIC_EX] = T{}.set_cracked ();
  a[MNEMONIC_EXRL] = T{}.set_cracked ();
  a[MNEMONIC_FLOGR] = T{}.set_groupalone ();
  a[MNEMONIC_IPM] = T{}.set_endgroup ();
  a[MNEMONIC_LAM] = T{}.set_expanded ();
  a[MNEMONIC_LCGFR] = T{}.set_cracked ();
  a[MNEMONIC_LCXBR] = T{}.set_groupalone ();
  a[MNEMONIC_LNGFR] = T{}.set_cracked ();
  a[MNEMONIC_LNXBR] = T{}.set_groupalone ();
  a[MNEMONIC_LPGFR] = T{}.set_cracked ();
  a[MNEMONIC_LPXBR] = T{}.set_groupalone ();
  a[MNEMONIC_LTXBR] = T{}.set_groupalone ();
  a[MNEMONIC_LTXTR] = T{}.set_groupalone ();
  a[MNEMONIC_LXDB] = T{}.set_groupalone ();
  a[MNEMONIC_LXDBR] = T{}.set_groupalone ();
  a[MNEMONIC_LXDTR] = T{}.set_groupalone ();
  a[MNEMONIC_LXEB] = T{}.set_groupalone ();
  a[MNEMONIC_LXEBR] = T{}.set_groupalone ();
  a[MNEMONIC_LXR] = T{}.set_cracked ();
  a[MNEMONIC_LZXR] = T{}.set_cracked ();
  a[MNEMONIC_MADB] = T{}.set_groupalone ();
  a[MNEMONIC_MADBR] = T{}.set_groupalone ();
  a[MNEMONIC_MAEB] = T{}.set_groupalone ();
  a[MNEMONIC_MAEBR] = T{}.set_groupalone ();
  a[MNEMONIC_MLG] = T{}.set_groupalone ();
  a[MNEMONIC_MLGR] = T{}.set_groupalone ();
  a[MNEMONIC_MSDB] = T{}.set_groupalone ();
  a[MNEMONIC_MSDBR] = T{}.set_groupalone ();
  a[MNEMONIC_MSEB] = T{}.set_groupalone ();
  a[MNEMONIC_MSEBR] = T{}.set_groupalone ();
  a[MNEMONIC_MVC] = T{}.set_cracked ().set_expanded ().set_groupalone ();
  a[MNEMONIC_MXBR] = T{}.set_groupalone ();
  a[MNEMONIC_MXTR] = T{}.set_groupalone ();
  a[MNEMONIC_NC] = T{}.set_cracked ().set_groupalone ();
  a[MNEMONIC_OC] = T{}.set_cracked ().set_groupalone ();
  a[MNEMONIC_SLB] = T{}.set_groupalone ();
  a[MNEMONIC_SLBG] = T{}.set_groupalone ();
  a[MNEMONIC_SLBGR] = T{}.set_groupalone ();
  a[MNEMONIC_SLBR] = T{}.set_groupalone ();
  a[MNEMONIC_SQXBR] = T{}.set_groupalone ();
  a[MNEMONIC_STAM] = T{}.set_expanded ();
  a[MNEMONIC_STMG] = T{}.set_expanded ();
  a[MNEMONIC_SXBR] = T{}.set_groupalone ();
  a[MNEMONIC_SXTR] = T{}.set_groupalone ();
  a[MNEMONIC_TCXB] = T{}.set_groupalone ();
  a[MNEMONIC_XC] = T{}.set_cracked ().set_groupalone ();
  return a;
}();
