/* { dg-do compile { target c++17 } } */
#include "allocate-allocator-handle.h"

/* Check application of align clause to static variables used in an OpenMP
   allocate directive in functions that are implicitly constexpr.
   Lambdas, lambdas in function templates.
   Missing cases for generics lambdas, see below.
   See allocate-16.C for cases in inline functions/function templates with
   -fimplicit-constexpr.

   This test case is valid in c++11 but the bug we are testing for does not
   manifest until c++17, which makes lambdas implicit constexpr functions.
   
   For now, we simply do not support these cases.  */



/* These cases had problems in c++17 and up because of dubious handling of
   constexpr functions in cp/decl.cc:make_rtl_for_nonlocal_decl.  In c++17 and
   up lambdas are implicitly constexpr, and are always considered potentially
   constexpr functions.  I think this is dubious as the constexpr flag does get
   set for a lambdas call operator, it also causes odd bugs where a lambda
   can be used in a constant expression, even when it should not be able to.
   But I digress, there is another test case for that and that oddity is not
   the root of this problem.

   Usually, make_rtl_for_nonlocal_decl defers compilation of local static
   variables until later, except when a function is declared constexpr.  From
   looking at why this code was originally added, I feel like it was a heavy
   handed fix for the original problem as it should have just made sure
   __func__, __PRETTY_FUNC__, and other static variables of that kind processed
   differently.  I would imagine that the thought process behind it went
   something like "well, static local variables aren't allowed in constexpr
   functions anyway, so this isn't a big deal" and I can understand this
   rationale.  With that said, __func__ and it's counterparts do not seem to be
   processed in make_rtl_for_nonlocal_decl anymore, so the added handling seems
   to be irrelevant now at best.

   Unfortunately, as stated in the first paragraph, because lambdas are
   unconditionally considered constexpr in c++17 and up, local static variables
   in lambdas are never deferred.  As far as I can tell, this has the
   consequence that they are always emitted regardless of whether they are used
   or not.  This really isn't that big a deal in the grand scheme of things,
   and it may very well be that they get pruned during LTO so the impact of
   this is probably slim to none.
   
   However, as soon as OpenMP allocate directives come into play, the problems
   are much more pronounced.  In non templates, early finalization occurs
   before we even parse the allocate directive, before the "omp allocate"
   attribute is added to the var decl.  Consequentially, the specified
   alignment in the align clause of the directive does not get applied to the
   variable.  In function templates the consequences are much worse, parsing
   the template decl adds an incomplete "omp allocate" attribute to the var
   decl as a marker in an incomplete state.  During instantiation of the
   template, early finalization of the decl occurs, before we have substituted
   into the allocate directive and finalized the attribute on the var decl.
   In this case, varpool_node::finalize_decl finds the "omp allocate" attribute
   and tries to read an alignment from it, which has not been stored yet.
   At best we crash here, at worst it reads garbage as an OMP_CLAUSE is stored
   there for diagnostic purposes.
   
   This also occurs for inline functions with -fimplicit-constexpr, but as
   noted above those tests are split out to allocate-16.C.  */



/* Making a regex for demangled identifiers is actually way harder than making
   a regex for mangled ones, too many escapes are needed.  */

/* { dg-final { scan-assembler "\.align 256\\s*\.type\\s*_ZZZ6f0_256vENKUlvE_clEvE1a" } } */
int* f0_256()
{
  auto cl = [](){
    static int a = 42;
    #pragma omp allocate(a) align(256) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
    return &a;
  };
  return cl();
}
/* { dg-final { scan-assembler "\.align 512\\s*\.type\\s*_ZZZ6f0_512vENKUlvE_clEvE1a" } } */
int* f0_512()
{
  auto cl = [](){
    static int a = 42;
    #pragma omp allocate(a) align(512) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
    return &a;
  };
  return cl();
}
/* { dg-final { scan-assembler "\.align 1024\\s*\.type\\s*_ZZZ7f0_1024vENKUlvE_clEvE1a" } } */
int* f0_1024()
{
  auto cl = [](){
    static int a = 42;
    #pragma omp allocate(a) align(1024) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
    return &a;
  };
  return cl();
}

/* { dg-final { scan-assembler "\.align 256\\s*\.type\\s*_ZZZ6f1_256IvEPivENKUlvE_clEvE1a" } } */
template<typename>
int* f1_256()
{
  auto cl = [](){
    static int a = 42;
    #pragma omp allocate(a) align(256) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
    return &a;
  };
  return cl();
}
template int* f1_256<void>();

/* { dg-final { scan-assembler "\.align 512\\s*\.type\\s*_ZZZ6f1_512IvEPivENKUlvE_clEvE1a" } } */
template<typename>
int* f1_512()
{
  auto cl = [](){
    static int a = 42;
    #pragma omp allocate(a) align(512) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
    return &a;
  };
  return cl();
}
template int* f1_512<void>();

/* { dg-final { scan-assembler "\.align 1024\\s*\.type\\s*_ZZZ7f1_1024IvEPivENKUlvE_clEvE1a" } } */
template<typename>
int* f1_1024()
{
  auto cl = [](){
    static int a = 42;
    #pragma omp allocate(a) align(1024) allocator(omp_default_mem_alloc) /* { dg-message "static variable 'a' is not supported in an 'allocate' directive in an implicit constexpr function" } */
    return &a;
  };
  return cl();
}
template int* f1_1024<void>();

/* Missing cases for generic lambda, and generic lambda in function template.
   They shouldn't behave differently, but for completeness they should be
   added, I'm just not going to spend any more time on this right now.  */
