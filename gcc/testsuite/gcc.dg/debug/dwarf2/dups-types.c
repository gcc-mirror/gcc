/* Test that these two options can work together.  */
/* { dg-options "-gdwarf-4 -dA -feliminate-dwarf2-dups -fdebug-types-section" } */
/* { dg-final { scan-assembler "DW.dups_types\.h\[^)\]*. DW_TAG_typedef" } } */
/* { dg-final { scan-assembler "DW_TAG_type_unit" } } */

#include "dups-types.h"

A2 a;
