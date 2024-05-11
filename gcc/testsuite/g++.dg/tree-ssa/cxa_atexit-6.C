/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fdump-tree-cddce1-details -fdump-tree-optimized -fPIC" } */
// { dg-require-effective-target cxa_atexit }
/* This test is not appropriate for targets where non-weak functions defined
   in the TU always bind locally; see PR114982.  */
/* { dg-skip-if "PR114982" { *-*-darwin* } } */
/* PR tree-optimization/19661 */

/* The call to axexit should not be removed as A::~A() cannot be figured if it
   is a pure/const function call for platforms where the function call g does
   not bind locally. */

__attribute__((noinline))
void g() {}

struct A { 
    A(); 
    ~A() { g(); } 
}; 
 
void foo () { 
  static A a; 
} 

/* { dg-final { scan-tree-dump-not "Deleting : (?:__cxxabiv1::__cxa_atexit|__aeabiv1::__aeabi_atexit)" "cddce1" } } */
/* { dg-final { scan-tree-dump-times "(?:__cxa_atexit|__aeabi_atexit)" 1 "optimized" } } */

