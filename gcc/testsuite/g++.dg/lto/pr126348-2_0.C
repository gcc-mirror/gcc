/* PR debug/126348 */
/* Verify that nested inlined subroutines and their direct children retain
   the same input TU provenance.  */
/* { dg-lto-do link } */
/* { dg-skip-if "No DWARF debug support" { hppa*-*-hpux* } } */
/* { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } } */
/* { dg-lto-options { { -O2 -g -gdwarf-5 -dA -save-temps -flto -flto-partition=one -fno-ipa-icf --param=max-inline-insns-auto=100 } } } */
/* { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" } */
/* { dg-final { scan-lto-assembler "DW_TAG_inlined_subroutine\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_2_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,10}\[^\n\]*DW_TAG_formal_parameter\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_2_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,8}\[^\n\]*DW_TAG_variable\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_2_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,3}\[^\n\]*DW_TAG_inlined_subroutine\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_2_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,10}\[^\n\]*DW_TAG_formal_parameter\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_2_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,8}\[^\n\]*DW_TAG_variable\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_2_1\[^\n\]*DW_AT_abstract_origin" } } */

#include "pr126348.h"

__attribute__ ((noinline)) int
one (const Buffer &buffer)
{
  return buffer.printable_length ();
}
