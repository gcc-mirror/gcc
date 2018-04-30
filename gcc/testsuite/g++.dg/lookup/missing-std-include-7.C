/* PR c++/85021: Verify that we suggest missing headers for common names in std::
   if there's a "using namespace std;" active.  */

/* No using-directive.  */

void test_1 ()
{
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-bogus "'<iostream>'" "" { target *-*-* } .-1 }
}

/* Local using-directive.  */

void test_2 ()
{
  using namespace std;
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-message "'std::cout' is defined in header '<iostream>'" "" { target *-*-* } .-1 }
}

/* Local using-directive, but not of "std".  */

namespace not_std {}
void test_3 ()
{
  using namespace not_std;
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-bogus "'<iostream>'" "" { target *-*-* } .-1 }
}

/* Local using-directive in wrong block.  */

void test_4 ()
{
  {
    using namespace std;
  }
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-bogus "'<iostream>'" "" { target *-*-* } .-1 }
}

/* Local using-directive used from nested block.  */

void test_5 ()
{
  using namespace std;

  for (int i = 0; i < 10; i++)
    {
      cout << "test"; // { dg-error "'cout' was not declared in this scope" }
      // { dg-message "'std::cout' is defined in header '<iostream>'" "" { target *-*-* } .-1 }
    }
}

namespace ns_1 {

namespace ns_2 {

using namespace std;

/* using-directive within the same namespace.  */

void test_6 ()
{
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-message "'std::cout' is defined in header '<iostream>'" "" { target *-*-* } .-1 }
}

namespace ns_3 {

/* Locate the using-directive within ns_2, the parent namespace.  */

void test_7 ()
{
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-message "'std::cout' is defined in header '<iostream>'" "" { target *-*-* } .-1 }
}

} // namespace ns_3
} // namespace ns_2

/* Back in ns_1, should not locate the using-directive.  */

void test_8 ()
{
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-bogus "'<iostream>'" "" { target *-*-* } .-1 }
}

} // namespace ns_1

/* using-directive in global namespace.  */
using namespace std;

void test_9 ()
{
  cout << "test"; // { dg-error "'cout' was not declared in this scope" }
  // { dg-message "'std::cout' is defined in header '<iostream>'" "" { target *-*-* } .-1 }
}

