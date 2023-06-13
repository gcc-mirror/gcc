void test (void)
{
  std::string s ("hello world"); // { dg-error ".string. is not a member of .std." }
  // { dg-message ".std::string. is defined in header .<string>.; this is probably fixable by adding .#include <string>." "" { target *-*-* } .-1 }

  std::wstring ws ("hello world"); // { dg-error ".wstring. is not a member of .std." }
  // { dg-message ".std::wstring. is defined in header .<string>.; this is probably fixable by adding .#include <string>." "" { target *-*-* } .-1 }

  std::cout << 10; // { dg-error ".cout. is not a member of .std." }
  // { dg-message ".std::cout. is defined in header .<iostream>.; this is probably fixable by adding .#include <iostream>." "" { target *-*-* } .-1 }

  int i;
  std::cin >> i; // { dg-error ".cin. is not a member of .std." }
  // { dg-message ".std::cin. is defined in header .<iostream>.; this is probably fixable by adding .#include <iostream>." "" { target *-*-* } .-1 }

  std::deque a; // { dg-error ".deque. is not a member of .std." }
  // { dg-message ".std::deque. is defined in header .<deque>.; this is probably fixable by adding .#include <deque>." "" { target *-*-* } .-1 }

  std::vector<int> v; // { dg-error ".vector. is not a member of .std." }
  // { dg-message ".std::vector. is defined in header .<vector>.; this is probably fixable by adding .#include <vector>." "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }

  std::list<int> lst;  // { dg-error ".list. is not a member of .std." }
  // { dg-message ".std::list. is defined in header .<list>.; this is probably fixable by adding .#include <list>." "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }

  std::pair<int,float> p; // { dg-error ".pair. is not a member of .std." }
  // { dg-message ".std::pair. is defined in header .<utility>.; this is probably fixable by adding .#include <utility>." "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }
}
