/* PR c++/57111 - Generalize -Wfree-nonheap-object to delete
   Verify that even without -Wsystem-headers the warning is issued
   for pairs of library functions defined in system headers.
   { dg-do compile { target c++11 } }
   { dg-options "-O2 -Wall" } */

#include <memory>
#include <string>

void test_string ()
{
  std::string str ("abc");          // { dg-message "declared here" }

  const char *s = str.c_str ();
  __builtin_printf ("%s\n", s);

  /* Because the delete call is made directly in the function this
     does not exercise the same thing as test_unique_ptr.  */
  delete s;                         // { dg-warning "'void operator delete\\(void\\*\[^\\)\]*\\)' called on unallocated object 'str'" }
}

void test_unique_ptr ()
{
  int arr[]= { 1, 2 };              // { dg-message "declared here" }

  std::unique_ptr<int[]> up (arr);
  __builtin_printf ("%i %i\n", up[0], up[1]);

  /* TO DO: verify that the warning is printed, including its inlining
     context (the directive below doesn't work):
     { Xdg-message "In member function.*inlined from 'void test_unique_ptr\\(\\)'.*warning: 'void operator delete \\\[]\\(void\\*\\)' called on unallocated object 'arr'" "" { target *-*-* } 0 }  */

  /* Here, the delete call is made indirectly from std::unique_ptr
     dtor.  */
}

/* Prune out the warning from test_unique_ptr().
   { dg-prune-output "-Wfree-nonheap-object" } */
