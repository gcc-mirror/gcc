/* { dg-skip-if "requires hosted libstdc++ for cstdlib free" { ! hostedlib } } */

#include <cstdlib>

struct s {};

void test_1 ()
{
  s *p = new s; // { dg-message "allocated here \\(expects deallocation with 'delete'\\)"
  free (p); // { dg-warning "'p' should have been deallocated with 'delete' but was deallocated with 'free'" }
}

void test_2 ()
{
  char *p = new char[16]; // { dg-message "allocated here \\(expects deallocation with 'delete\\\[\\\]'\\)"
  free (p); // { dg-warning "'p' should have been deallocated with 'delete\\\[\\\]' but was deallocated with 'free'" }
}

void test_3 ()
{
  char *p = (char *)malloc (16); // { dg-message "allocated here \\(expects deallocation with 'free'\\)"
  delete[] p; // { dg-warning "'p' should have been deallocated with 'free' but was deallocated with 'delete\\\[\\\]'" }
}
