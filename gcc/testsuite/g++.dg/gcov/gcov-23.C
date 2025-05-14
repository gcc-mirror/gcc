/* { dg-options "-fpath-coverage --coverage" } */
/* { dg-do run { target native } } */

#include "gcov-23-1.h"
#include "gcov-23-2.h"

/* This tests a very specific interaction between destructors of static objects
   and gcov's inlining aware printing of prime paths.  The source of the
   problem is that gcc will reuse the compiler generated destructor gimple stmt
   from the first static-object destructor, and its location will be recorded
   in the .gcno file.  This would lead to an empty block.locations vector since
   the stmt is not anchored to a source line (being generated and all), but the
   block itself would be originating in a different file.

   Without properly guarding the block.loc.lines access this code caused a
   segfault in gcov.  The class needed a non-trivial member or otherwise
   non-trivial constructor to emit approximately

    __cxxabiv1::__cxa_atexit (__dt_comp , &self, &__dso_handle);

    Triggering the failure needed the path to also be printed, so it is
    important to request both covered and uncovered paths.  */

int main ()
{
  fst::instance().data = "foo";
  snd::instance().data = "bar";
}

/* { dg-final { run-gcov { --prime-paths-source=both --prime-paths-lines=both gcov-23.C } } } */
