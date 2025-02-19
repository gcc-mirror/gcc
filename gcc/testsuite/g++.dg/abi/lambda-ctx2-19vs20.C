// { dg-do compile { target c++17 } }
// { dg-options "-fabi-version=20 -Wabi=19" }

#include "lambda-ctx2.h"

// { dg-regexp {[^\n]*lambda-ctx2.h:[:0-9]* warning: the mangled name of .A::<lambda>.[^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx2.h:[:0-9]* warning: the mangled name of .B<int>::<lambda>.[^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx2.h:[:0-9]* warning: the mangled name of .B<int>::<lambda>.[^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx2.h:[:0-9]* warning: the mangled name of .D::<lambda>.[^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx2.h:[:0-9]* warning: the mangled name of .E<int>::<lambda>.[^\n]*\n} }
// { dg-regexp {[^\n]*lambda-ctx2.h:[:0-9]* warning: the mangled name of .E<int>::<lambda>.[^\n]*\n} }
