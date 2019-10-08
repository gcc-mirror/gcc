/* PR c++/92001 - missing -Wclass-memaccess with array as first argument
   to memset
   { dg-do compile }
   { dg-options "-Wall" } */

extern "C" void* memset (void*, int, __SIZE_TYPE__);

struct S { S (); };

void test_array_access (S *p, S (*pa)[2], S (&r)[3])
{
  S a[1];
  memset (a, 0, sizeof a);        // { dg-warning "-Wclass-memaccess" }

  memset (*pa, 0, sizeof *pa);    // { dg-warning "-Wclass-memaccess" }

  memset (r, 0, sizeof r);        // { dg-warning "-Wclass-memaccess" }
}
