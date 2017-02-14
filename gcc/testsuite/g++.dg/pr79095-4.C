/* { dg-do compile } */
/* { dg-options "-Wall -O3 -fdump-tree-vrp2" } */

#include <vector>

void foo(std::vector<unsigned int> &v);

void vtest()
{
  std::vector<unsigned int> v;
  foo (v);
  {
    v.resize (v.size()-1);
  }
}

/* As written this testcase should trigger a warning.  We overflow to -1U
   if v.size() == 0 in foo().  This results in bogus calls to memset.

   The number of clearing loops in the IL can vary depending on the C++
   mode used for the test.  But by the end of VRP2, there should be a single
   clearing loop left and it should be using memcpy.  */
/* { dg-final { scan-tree-dump-times  "__builtin_memset \\(_\[0-9\]+, 0, \[0-9\]+\\)" 1 "vrp2" } } */

/* And that call should trigger a warning.  */
/* { dg-warning "exceeds maximum object size" "" { target *-*-* } 0 } */ 
