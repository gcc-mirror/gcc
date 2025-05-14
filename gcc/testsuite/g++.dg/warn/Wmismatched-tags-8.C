/* Verify that #pragma GCC diagnostic works for -Wmismatched-tags.
   { dg-do "compile" }
   { dg-options "-Wmismatched-tags" } */

#pragma GCC diagnostic push
#pragma GCC diagnostic error "-Wmismatched-tags"
class A;            // { dg-message "first declared" }
struct A;           // { dg-error "\\\[-Werror=mismatched-tags" }

#pragma GCC diagnostic ignored "-Wmismatched-tags"
class B;            // { dg-bogus "first declared" }
struct B;

#pragma GCC diagnostic warning "-Wmismatched-tags"
class C;            // { dg-message "first declared" }
struct C;           // { dg-warning "\\\[-Wmismatched-tags" }
#pragma GCC diagnostic pop

class D;            // { dg-message "first declared" }
struct D;           // { dg-warning "\\\[-Wmismatched-tags" }

// { dg-prune-output "some warnings being treated as errors" }
