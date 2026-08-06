// Verify template parameter names, names of arguments and block scope decls
// in functions and function templates are uglified and don't contain
// badnames.
// { dg-options "-O0 -std=c++29 -freflection" }

#include <bits/stdc++.h>

// { dg-bogus "note: non-uglified name" "" { target *-*-* } 0 }
// { dg-bogus "note: badname" "" { target *-*-* } 0 }
