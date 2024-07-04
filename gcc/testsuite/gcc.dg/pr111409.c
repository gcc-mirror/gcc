/* { dg-do compile } */
/* { dg-skip-if "split DWARF unsupported" { hppa*-*-hpux* powerpc*-ibm-aix* *-*-darwin* } } */
/* { dg-options "-gsplit-dwarf -g3 -dA" } */
/* { dg-final { scan-assembler-times {\.section\t"?\.debug_macro} 1 } } */
/* { dg-final { scan-assembler-not {\.byte\s+0x7\s*#\s*Import} } } */

#define foo 1
