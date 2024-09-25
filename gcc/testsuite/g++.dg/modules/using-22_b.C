// { dg-additional-options "-fmodules-ts" }

import std;

int main()
{
  std::basic_string<char> s;

  // The inline namespace should not be exported, only the 'using' in std.
  std::__cxx11::basic_string<char> s2; // { dg-error "has not been declared" }
  // The non-exported using should also not be visible.
  foo::basic_string<char> s3; // { dg-error "has not been declared" }
}

// { dg-prune-output "expected primary-expression" }
