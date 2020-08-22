/* Verify that -Wmismatched-tags doesn't print stray notes for warnings
   disabled in system headers.
  { dg-do "compile" }
  { dg-options "-Wmismatched-tags" } */

# 6 "Wmismatched-tags-7.C" 1
# 1 "system-header.h" 1 3 4
# 9 "system-header.h" 3 4
class A;            // { dg-bogus "first declared" }
struct A;           // { dg-bogus "replace" }
# 12 "Wmismatched-tags-7.C" 2
class B;            // { dg-message "first declared" }
struct B;           // { dg-warning "\\\[-Wmismatched-tags" }
