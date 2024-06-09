/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1-details -fdump-tree-optimized" } */
// { dg-require-effective-target cxa_atexit }
/* PR tree-optimization/19661 */

/* We should not remove the call to atexit as A::~A is unknown.  */

struct A { 
    A(); 
    ~A();
}; 

void foo () { 
  static A a; 
} 

/* { dg-final { scan-tree-dump-not "Deleting : (?:__cxxabiv1::__cxa_atexit|__aeabiv1::__aeabi_atexit)" "cddce1" } } */
/* { dg-final { scan-tree-dump-times "(?:__cxa_atexit|__aeabi_atexit)" 1 "optimized" } } */

