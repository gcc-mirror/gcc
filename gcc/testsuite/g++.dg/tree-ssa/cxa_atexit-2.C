/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -fdump-tree-cddce1-details -fdump-tree-optimized" } */
// { dg-require-effective-target cxa_atexit }
/* PR tree-optimization/19661 */

/* The call to axexit should be not removed as A::~A() as it marked with noipa.  */

struct A { 
    A(); 
    ~A();
}; 

[[gnu::noipa]] A::~A() {}
 
void foo () { 
  static A a; 
} 

/* { dg-final { scan-tree-dump-not "Deleting : (?:__cxxabiv1::__cxa_atexit|__aeabiv1::__aeabi_atexit)" "cddce1" } } */
/* { dg-final { scan-tree-dump-times "(?:__cxa_atexit|__aeabi_atexit)" 1 "optimized" } } */

