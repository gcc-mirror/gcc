/* Exception handling constructs in dead code, '-mfake-exceptions'.  */

/* { dg-do run } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-options -fexceptions } */
/* { dg-additional-options -mfake-exceptions }
   { dg-bogus {sorry, unimplemented: exception handling not supported} {} { target *-*-* } 0 } */
/* { dg-additional-options -O0 } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "exceptions-pr118794-1.C"

/* In this specific C++ arrangement, distilled from PR118794, GCC synthesizes
   '__builtin_eh_pointer', '__builtin_unwind_resume' calls as dead code in 'f':
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_eh_pointer, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_unwind_resume, } 1 optimized } } */
