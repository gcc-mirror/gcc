/* { dg-do compile } */
/* { dg-skip-if "No dwarf debug support" { hppa*-*-hpux* } } */
/* { dg-options "-O2 -g -gdwarf-5 -gsplit-dwarf" } */

/* No addresses in the DWARF, so no .debug_addr section,
   don't crash trying to generate an addr index header anyway.  */

namespace { struct t {}; }
t f () { return t (); }
