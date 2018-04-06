void test (void)
{
  std::string s ("hello world"); // { dg-error ".string. is not a member of .std." }
  // { dg-message ".std::string. is defined in header .<string>.; did you forget to .#include <string>.?" "" { target *-*-* } .-1 }

  std::wstring ws ("hello world"); // { dg-error ".wstring. is not a member of .std." }
  // { dg-message ".std::wstring. is defined in header .<string>.; did you forget to .#include <string>.?" "" { target *-*-* } .-1 }

  std::cout << 10; // { dg-error ".cout. is not a member of .std." }
  // { dg-message ".std::cout. is defined in header .<iostream>.; did you forget to .#include <iostream>.?" "" { target *-*-* } .-1 }

  int i;
  std::cin >> i; // { dg-error ".cin. is not a member of .std." }
  // { dg-message ".std::cin. is defined in header .<iostream>.; did you forget to .#include <iostream>.?" "" { target *-*-* } .-1 }

  std::deque a; // { dg-error ".deque. is not a member of .std." }
  // { dg-message ".std::deque. is defined in header .<deque>.; did you forget to .#include <deque>.?" "" { target *-*-* } .-1 }

  std::vector<int> v; // { dg-error ".vector. is not a member of .std." }
  // { dg-message ".std::vector. is defined in header .<vector>.; did you forget to .#include <vector>.?" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }

  std::list<int> lst;  // { dg-error ".list. is not a member of .std." }
  // { dg-message ".std::list. is defined in header .<list>.; did you forget to .#include <list>.?" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }

  std::pair<int,float> p; // { dg-error ".pair. is not a member of .std." }
  // { dg-message ".std::pair. is defined in header .<utility>.; did you forget to .#include <utility>.?" "" { target *-*-* } .-1 }
  // { dg-error "expected primary-expression before .int." "" { target *-*-* } .-2 }
}
