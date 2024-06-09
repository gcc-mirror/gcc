/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1-details -fdump-tree-optimized" } */
// { dg-require-effective-target cxa_atexit }
/* PR tree-optimization/19661 */

/* The call to axexit should be removed as A::~A() is a pure/const function call
   and there is no visible effect if A::~A() call does not happen.  */

struct A { 
    A(); 
    ~A() {} 
}; 
 
void foo () { 
  static A a; 
} 

/* { dg-final { scan-tree-dump-times "Deleting : (?:__cxxabiv1::__cxa_atexit|__aeabiv1::__aeabi_atexit)" 1 "cddce1" } } */
/* { dg-final { scan-tree-dump-not "__cxa_atexit|__aeabi_atexit" "optimized" } } */

