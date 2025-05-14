/* Exception handling constructs in dead code.  */

/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-options -fexceptions } */
/* { dg-additional-options -mno-fake-exceptions } */
/* { dg-additional-options -O0 } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "../../../../libgomp/testsuite/libgomp.c++/target-exceptions-pr118794-1.C"

/* In this specific C++ arrangement, distilled from PR118794, GCC synthesizes
   '__builtin_eh_pointer', '__builtin_unwind_resume' calls as dead code in 'f':
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_eh_pointer, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_unwind_resume, } 1 optimized } }
   Given '-O0', compilation fails:
   { dg-regexp {[^\r\n]+: In function 'void f\(\)':[\r\n]+(?:[^\r\n]+: sorry, unimplemented: exception handling not supported[\r\n]+)+} }
   (Note, using 'dg-regexp' instead of 'dg-message', as the former runs before the auto-mark-UNSUPPORTED.)  */
