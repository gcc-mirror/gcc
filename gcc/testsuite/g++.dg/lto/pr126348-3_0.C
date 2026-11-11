/* PR debug/126348 */
/* Different options keep the COMDAT declarations distinct until prevailing
   decl replacement.  That replacement must not invalidate the split
   function's origin from pr126348-3_1.C.  */
/* { dg-lto-do link } */
/* { dg-skip-if "No DWARF debug support" { hppa*-*-hpux* } } */
/* { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } } */
/* { dg-lto-options { { -O2 -g -gdwarf-5 -dA -save-temps -flto -flto-partition=one -fno-ipa-icf --param=max-inline-insns-auto=100 } } } */
/* { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" } */
/* { dg-final { scan-lto-assembler "DW_TAG_subprogram\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_3_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,4}\[^\n\]*DW_AT_(?:low_pc|ranges)(?:\[^\n\]*\n){1,8}\[^\n\]*DW_TAG_formal_parameter\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_3_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,4}\[^\n\]*DW_TAG_variable\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_3_1\[^\n\]*DW_AT_abstract_origin" } } */

#include "pr126348-3.h"

__attribute__ ((noinline)) int
one (const Buffer &buffer)
{
  return buffer.printable_length ();
}

__attribute__ ((noinline)) int
one_extra (const Buffer &buffer)
{
  return buffer.printable_length ();
}
