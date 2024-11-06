/* The goal here is to ensure that we never consider a call to a noreturn
   function as a potential tail call.

   Right now GCC discovers potential tail calls by looking at the
   predecessors of the exit block.  A call to a non-return function
   has no successors and thus can never match that first filter.

   But that could change one day and we want to catch it.  The problem
   is the compiler could potentially optimize a tail call to a nonreturn
   function, even if the caller has a frame.  That breaks the assumption
   that calls probe *sp when saving the return address that some targets
   depend on to elide stack probes.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-tree-tailc -fdump-tree-optimized -fno-ipa-icf" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

extern void foo (void) __attribute__ ((__noreturn__));


void
test_direct_1 (void)
{
  foo ();
}

void
test_direct_2 (void)
{
  return foo ();
}

void (*indirect)(void)__attribute__ ((noreturn));


void
test_indirect_1 ()
{
  (*indirect)();
}

void
test_indirect_2 (void)
{
  return (*indirect)();;
}


typedef void (*pvfn)() __attribute__ ((noreturn));

void (*indirect_casted)(void);

void
test_indirect_casted_1 ()
{
  (*(pvfn)indirect_casted)();
}

void
test_indirect_casted_2 (void)
{
  return (*(pvfn)indirect_casted)();
}
/* { dg-final { scan-tree-dump-not "tail call" "tailc" } } */
/* { dg-final { scan-tree-dump-not "tail call" "optimized" } } */

