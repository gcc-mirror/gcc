// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98067
// { dg-options "-gdwarf-2 -gstrict-dwarf -I $srcdir/gdc.dg/debug/dwarf2" }
// { dg-do compile }
module pr98067;

import imports.pr98067 : MAP_ANON;
