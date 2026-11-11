/* PR debug/126348 */
/* Verify that an out-of-line split function and its direct children retain
   the same input TU provenance, and that the function has an address-bearing
   late DIE.  */
/* { dg-lto-do link } */
/* { dg-skip-if "No DWARF debug support" { hppa*-*-hpux* } } */
/* { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } } */
/* { dg-lto-options { { -O2 -g -gdwarf-5 -dA -save-temps -flto -flto-partition=one -fno-ipa-icf --param=max-inline-insns-auto=100 } } } */
/* { dg-extra-ld-options "-r -nostdlib -flinker-output=nolto-rel" } */
/* { dg-final { scan-lto-assembler "DW_TAG_subprogram\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_0\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,4}\[^\n\]*DW_AT_(?:low_pc|ranges)(?:\[^\n\]*\n){1,8}\[^\n\]*DW_TAG_formal_parameter\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_0\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,4}\[^\n\]*DW_TAG_variable\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_0\[^\n\]*DW_AT_abstract_origin" } } */
/* { dg-final { scan-lto-assembler "DW_TAG_subprogram\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,4}\[^\n\]*DW_AT_(?:low_pc|ranges)(?:\[^\n\]*\n){1,8}\[^\n\]*DW_TAG_formal_parameter\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,4}\[^\n\]*DW_TAG_variable\\)(?:\[^\n\]*\n){1,6}\[^\n\]*pr126348_1\[^\n\]*DW_AT_abstract_origin" } } */

/* { dg-final { scan-lto-assembler "DW_TAG_inlined_subroutine\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,10}\[^\n\]*DW_TAG_formal_parameter\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_1\[^\n\]*DW_AT_abstract_origin(?:\[^\n\]*\n){1,8}\[^\n\]*DW_TAG_variable\\)(?:\[^\n\]*\n){1,3}\[^\n\]*pr126348_1\[^\n\]*DW_AT_abstract_origin" } } */

#include "pr126348.h"

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
