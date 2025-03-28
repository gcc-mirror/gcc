/* 'throw', caught.  */

/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-options -fexceptions } */
/* { dg-additional-options -mno-fake-exceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "../../../../libgomp/testsuite/libgomp.oacc-c++/exceptions-throw-2.C"

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   Compilation fails:
   { dg-regexp {[^\r\n]+: In function 'int main\(\)':[\r\n]+(?:[^\r\n]+: sorry, unimplemented: exception handling not supported[\r\n]+)+} }
   (Note, using 'dg-regexp' instead of 'dg-message', as the former runs before the auto-mark-UNSUPPORTED.)  */
