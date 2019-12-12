// { dg-options "-gdwarf-2" }
/* { dg-skip-if "No Dwarf" { { *-*-aix* hppa*-*-hpux* } && { ! hppa*64*-*-* } } } */

extern "C" long double nanl(const char *);
using ::nanl;

// We should elide the using for this extern C builtin
// { dg-final { scan-assembler-not ".debug_info" } }
