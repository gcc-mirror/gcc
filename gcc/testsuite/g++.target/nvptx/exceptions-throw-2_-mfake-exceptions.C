/* 'throw', caught, '-mfake-exceptions'.  */

/* { dg-do run } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-options -fexceptions } */
/* { dg-additional-options -mfake-exceptions }
   { dg-bogus {sorry, unimplemented: exception handling not supported} {} { target *-*-* } 0 } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "exceptions-throw-2.C"

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   We don't print anything, but just 'abort'.

   There is no 'catch'ing; any exception is fatal.
   { dg-shouldfail {'MyException' exception} } */
