// { dg-do compile { target c++11 } }
/* PIC uses .data.rel.ro.local rather than .rodata.  */
/* { dg-additional-options "-fno-PIE" } */

#include <initializer_list>

const auto x = { 1, 2 };

// { dg-final { scan-assembler-not {\.data} { xfail hppa*-*-hpux* powerpc-ibm-aix* } } }
