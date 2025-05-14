/* 'std::bad_cast' exception.  */

/* { dg-do run } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */
/* { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "../../../../libgomp/testsuite/libgomp.oacc-c++/exceptions-bad_cast-1.C"

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   We don't print anything, but just 'abort'.

   { dg-shouldfail {'std::bad_cast' exception} } */
