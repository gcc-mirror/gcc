// Verify our -Wparentheses warning handles "boolish" class types
// such as std::vector<bool>'s reference type the same as ordinary
// bool.
// { dg-additional-options "-Wparentheses" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

void f(std::vector<bool> v, int i) {
  bool b;
  b = v[i] = true;
  b = v[i] = v[i+1];

  if (v[i] = 42) { }     // { dg-message "parentheses" }
  if (v[i] = v[i+1]) { } // { dg-message "parentheses" }

  if ((v[i] = 42)) { }
  if ((v[i] = v[i+1])) { }
}

template<class>
void ft(std::vector<bool> v, int i) {
  bool b;
  b = v[i] = true;
  b = v[i] = v[i+1];

  if (v[i] = 42) { }     // { dg-message "parentheses" }
  if (v[i] = v[i+1]) { } // { dg-message "parentheses" }

  if ((v[i] = 42)) { }
  if ((v[i] = v[i+1])) { }
}
